//! Build-time TypeScript/JavaScript precompiler
//!
//! Compiles handler files to bytecode at build time for embedding in the binary.
//! This eliminates runtime parsing overhead and removes the need for source files
//! in deployment.
//!
//! Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--sql-schema path] [--prove spec] [--policy policy.json] <handler.ts> <output.zig>

const std = @import("std");
const builtin = @import("builtin");
const zts = @import("zts");
const ir = zts.parser;
const IrTranspiler = @import("transpiler.zig").IrTranspiler;
const handler_contract = zts.handler_contract;
const ContractBuilder = handler_contract.ContractBuilder;
const writeContractJson = handler_contract.writeContractJson;
const HandlerContract = handler_contract.HandlerContract;
const VerificationInfo = handler_contract.VerificationInfo;
const handler_policy = zts.handler_policy;
const HandlerPolicy = handler_policy.HandlerPolicy;
const deploy_manifest = @import("deploy_manifest.zig");
const openapi_manifest = @import("openapi_manifest.zig");
const prove_upgrade = @import("prove_upgrade.zig");
const sqlite = zts.sqlite;
const sql_analysis = zts.sql_analysis;

const AotAnalysis = struct {
    dispatch: ?*zts.PatternDispatchTable = null,
    default_response: ?zts.HandlerAnalyzer.StaticResponseInfo = null,
    handler_loc: ?zts.parser.SourceLocation = null,

    fn deinit(self: *AotAnalysis, allocator: std.mem.Allocator) void {
        if (self.dispatch) |dispatch| {
            dispatch.deinit();
            allocator.destroy(dispatch);
        }
        if (self.default_response) |resp| {
            if (resp.body.len > 0) {
                allocator.free(resp.body);
            }
        }
    }
};

const CompiledHandler = struct {
    bytecode: []const u8,
    aot: ?AotAnalysis = null,
    transpiled_source: ?[]const u8 = null,
    /// Additional dependency module bytecodes (for file imports).
    /// Stored in execution order, entry module is NOT included.
    dep_bytecodes: ?[]const []const u8 = null,
    /// Contract manifest (when --contract is passed)
    contract: ?HandlerContract = null,
    /// Generated test JSONL (when --generate-tests is passed)
    generated_tests: ?[]const u8 = null,

    fn deinit(self: *CompiledHandler, allocator: std.mem.Allocator) void {
        if (self.aot) |*analysis| {
            analysis.deinit(allocator);
        }
        if (self.dep_bytecodes) |deps| {
            for (deps) |dep| {
                allocator.free(dep);
            }
            allocator.free(deps);
        }
        if (self.contract) |*c| {
            c.deinit(allocator);
        }
        if (self.generated_tests) |gt| {
            allocator.free(gt);
        }
        if (self.bytecode.len > 0) {
            allocator.free(self.bytecode);
        }
        // Note: transpiled_source is owned by the transpiler, not freed here
    }
};

const readFilePosix = zts.file_io.readFile;

/// Write a file synchronously using posix operations
pub fn writeFilePosix(path: []const u8, data: []const u8, allocator: std.mem.Allocator) !void {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o644,
    );
    defer std.Io.Threaded.closeFd(fd);

    var total_written: usize = 0;
    while (total_written < data.len) {
        const result = std.c.write(fd, data[total_written..].ptr, data.len - total_written);
        if (result < 0) return error.WriteFailure;
        if (result == 0) return error.WriteFailure;
        total_written += @intCast(result);
    }
}

const PrecompileOptions = struct {
    handler_path: []const u8,
    output_path: []const u8,
    emit_aot: bool = false,
    emit_verify: bool = false,
    emit_contract: bool = false,
    emit_openapi: bool = false,
    sql_schema_path: ?[]const u8 = null,
    policy_path: ?[]const u8 = null,
    deploy_target_str: ?[]const u8 = null,
    replay_trace_path: ?[]const u8 = null,
    test_file_path: ?[]const u8 = null,
    prove_spec: ?[]const u8 = null,
    generate_tests: bool = false,
};

fn parsePrecompileArgs(args_vector: std.process.Args) !PrecompileOptions {
    var args = std.process.Args.Iterator.init(args_vector);
    defer args.deinit();
    _ = args.skip();

    var opts = PrecompileOptions{ .handler_path = "", .output_path = "" };
    var handler_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--aot")) { opts.emit_aot = true; continue; }
        if (std.mem.eql(u8, arg, "--verify")) { opts.emit_verify = true; continue; }
        if (std.mem.eql(u8, arg, "--generate-tests")) { opts.generate_tests = true; continue; }
        if (std.mem.eql(u8, arg, "--contract")) { opts.emit_contract = true; continue; }
        if (std.mem.eql(u8, arg, "--openapi")) { opts.emit_openapi = true; continue; }
        if (std.mem.eql(u8, arg, "--sql-schema")) {
            opts.sql_schema_path = args.next() orelse {
                std.debug.print("Missing path after --sql-schema\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--policy")) {
            opts.policy_path = args.next() orelse {
                std.debug.print("Missing path after --policy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--deploy")) {
            opts.deploy_target_str = args.next() orelse {
                std.debug.print("Missing target after --deploy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--replay")) {
            opts.replay_trace_path = args.next() orelse {
                std.debug.print("Missing path after --replay\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--test-file")) {
            opts.test_file_path = args.next() orelse {
                std.debug.print("Missing path after --test-file\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--prove")) {
            opts.prove_spec = args.next() orelse {
                std.debug.print("Missing spec after --prove (format: contract.json or contract.json:traces.jsonl)\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (handler_path == null) { handler_path = arg; continue; }
        if (output_path == null) { output_path = arg; continue; }
        std.debug.print("Unexpected argument: {s}\n", .{arg});
        return error.InvalidArgument;
    }

    const usage = "Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--sql-schema path] [--prove spec] [--policy policy.json] <handler.ts> <output.zig>\n";
    opts.handler_path = handler_path orelse {
        std.debug.print(usage, .{});
        std.debug.print("\nCompiles a TypeScript/JavaScript handler to bytecode.\n", .{});
        return error.MissingArgument;
    };
    opts.output_path = output_path orelse {
        std.debug.print(usage, .{});
        std.debug.print("\nMissing output path.\n", .{});
        return error.MissingArgument;
    };

    return opts;
}

pub fn main(init: std.process.Init.Minimal) !void {
    var debug_alloc: if (builtin.mode == .Debug) std.heap.DebugAllocator(.{}) else void =
        if (builtin.mode == .Debug) .init else {};
    defer if (builtin.mode == .Debug) {
        _ = debug_alloc.deinit();
    };
    const allocator = if (builtin.mode == .Debug) debug_alloc.allocator() else std.heap.smp_allocator;

    const opts = parsePrecompileArgs(init.args) catch |err| {
        if (err == error.MissingArgument) return;
        return err;
    };

    const handler_path_final = opts.handler_path;
    const output_path_final = opts.output_path;
    const emit_aot = opts.emit_aot;
    const emit_verify = opts.emit_verify;
    const emit_contract = opts.emit_contract;
    const emit_openapi = opts.emit_openapi;
    const sql_schema_path = opts.sql_schema_path;
    const policy_path = opts.policy_path;
    const deploy_target_str = opts.deploy_target_str;
    const replay_trace_path = opts.replay_trace_path;
    const test_file_path = opts.test_file_path;
    const prove_spec = opts.prove_spec;

    // Read the handler source file (using posix for synchronous I/O)
    const source = readFilePosix(allocator, handler_path_final, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading handler file '{s}': {}\n", .{ handler_path_final, err });
        return err;
    };
    defer allocator.free(source);

    var policy: ?HandlerPolicy = null;
    defer if (policy) |*p| p.deinit(allocator);
    if (policy_path) |path| {
        const policy_source = readFilePosix(allocator, path, 1024 * 1024) catch |err| {
            std.debug.print("Error reading policy file '{s}': {}\n", .{ path, err });
            return err;
        };
        defer allocator.free(policy_source);

        policy = handler_policy.parsePolicyJson(allocator, policy_source) catch |err| {
            std.debug.print("Error parsing policy file '{s}': {}\n", .{ path, err });
            return err;
        };
    }

    std.debug.print("Compiling handler: {s} ({d} bytes)\n", .{ handler_path_final, source.len });

    // Compile the handler to bytecode (+ optional AOT analysis + optional verification + optional contract)
    const generate_tests = opts.generate_tests;

    var compiled = compileHandler(
        allocator,
        source,
        handler_path_final,
        emit_aot,
        emit_verify,
        emit_contract,
        policy,
        sql_schema_path,
        generate_tests,
    ) catch |err| {
        std.debug.print("Compilation failed: {}\n", .{err});
        return err;
    };
    defer compiled.deinit(allocator);

    std.debug.print("Generated bytecode: {d} bytes\n", .{compiled.bytecode.len});
    if (compiled.aot != null) {
        std.debug.print("AOT analysis enabled\n", .{});
    }

    // Run replay verification if --replay was passed.
    // This replays recorded traces against the handler and fails the build
    // if any regressions are detected.
    if (replay_trace_path) |trace_path| {
        const trace_source = readFilePosix(allocator, trace_path, 100 * 1024 * 1024) catch |err| {
            std.debug.print("Error reading trace file '{s}': {}\n", .{ trace_path, err });
            return err;
        };
        defer allocator.free(trace_source);

        const groups = zts.trace.parseTraceFile(allocator, trace_source) catch |err| {
            std.debug.print("Error parsing trace file '{s}': {}\n", .{ trace_path, err });
            return err;
        };
        defer {
            for (groups) |g| allocator.free(g.io_calls);
            allocator.free(groups);
        }

        if (groups.len == 0) {
            std.debug.print("Warning: no traces found in '{s}'\n", .{trace_path});
        } else {
            std.debug.print("Replaying {d} traces for regression verification...\n", .{groups.len});
            const replay_result = runBuildTimeReplay(allocator, source, handler_path_final, groups) catch |err| {
                std.debug.print("Replay verification failed: {}\n", .{err});
                return err;
            };
            std.debug.print("Replay: {d}/{d} identical", .{ replay_result.pass, replay_result.total });
            if (replay_result.fail > 0) {
                std.debug.print(", {d} REGRESSIONS DETECTED", .{replay_result.fail});
            }
            std.debug.print("\n", .{});
            if (replay_result.fail > 0) {
                std.debug.print("Build aborted: replay verification failed.\n", .{});
                return error.ReplayVerificationFailed;
            }
        }
    }

    // Run handler tests if --test-file was passed.
    if (test_file_path) |t_path| {
        const test_source = readFilePosix(allocator, t_path, 100 * 1024 * 1024) catch |err| {
            std.debug.print("Error reading test file '{s}': {}\n", .{ t_path, err });
            return err;
        };
        defer allocator.free(test_source);

        const test_result = runBuildTimeTests(allocator, source, handler_path_final, test_source) catch |err| {
            std.debug.print("Build-time test execution failed: {}\n", .{err});
            return err;
        };
        std.debug.print("Tests: {d}/{d} passed", .{ test_result.pass, test_result.total });
        if (test_result.fail > 0) {
            std.debug.print(", {d} FAILED", .{test_result.fail});
        }
        std.debug.print("\n", .{});
        if (test_result.fail > 0) {
            std.debug.print("Build aborted: handler tests failed.\n", .{});
            return error.TestsFailed;
        }
    }

    // Write the output Zig file
    writeZigFile(output_path_final, compiled, handler_path_final, policy, allocator) catch |err| {
        std.debug.print("Error writing output file '{s}': {}\n", .{ output_path_final, err });
        return err;
    };

    std.debug.print("Wrote embedded handler to: {s}\n", .{output_path_final});

    // Print sandbox report when auto-deriving from contract (no explicit policy)
    if (policy == null) {
        if (compiled.contract) |*contract| {
            printSandboxReport(contract);
        }
    }

    // Print handler effect properties
    if (compiled.contract) |*contract| {
        printPropertiesReport(contract);
    }

    if (compiled.contract) |*contract| {
        // Write contract.json alongside the output if requested
        if (emit_contract) {
            const contract_path = deriveSiblingPath(allocator, output_path_final, "contract.json") catch |err| {
                std.debug.print("Error deriving contract path: {}\n", .{err});
                return err;
            };
            defer allocator.free(contract_path);

            var json_output: std.ArrayList(u8) = .empty;
            defer json_output.deinit(allocator);
            var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &json_output);

            writeContractJson(contract, &aw.writer) catch |err| {
                std.debug.print("Error serializing contract: {}\n", .{err});
                return err;
            };
            json_output = aw.toArrayList();

            writeFilePosix(contract_path, json_output.items, allocator) catch |err| {
                std.debug.print("Error writing contract file '{s}': {}\n", .{ contract_path, err });
                return err;
            };

            std.debug.print("Wrote contract manifest to: {s}\n", .{contract_path});
        }

        if (emit_openapi) {
            const openapi_path = deriveSiblingPath(allocator, output_path_final, "openapi.json") catch |err| {
                std.debug.print("Error deriving OpenAPI path: {}\n", .{err});
                return err;
            };
            defer allocator.free(openapi_path);

            var openapi_output: std.ArrayList(u8) = .empty;
            defer openapi_output.deinit(allocator);
            var openapi_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &openapi_output);

            openapi_manifest.writeOpenApiJson(&openapi_aw.writer, contract, .{}) catch |err| {
                std.debug.print("Error serializing OpenAPI manifest: {}\n", .{err});
                return err;
            };
            openapi_output = openapi_aw.toArrayList();

            writeFilePosix(openapi_path, openapi_output.items, allocator) catch |err| {
                std.debug.print("Error writing OpenAPI file '{s}': {}\n", .{ openapi_path, err });
                return err;
            };

            std.debug.print("Wrote OpenAPI manifest to: {s}\n", .{openapi_path});
        }

        // Write generated tests if --generate-tests was passed
        if (compiled.generated_tests) |tests_jsonl| {
            const tests_path = deriveSiblingPath(allocator, output_path_final, "handler.auto-tests.jsonl") catch |err| {
                std.debug.print("Error deriving test output path: {}\n", .{err});
                return err;
            };
            defer allocator.free(tests_path);

            writeFilePosix(tests_path, tests_jsonl, allocator) catch |err| {
                std.debug.print("Error writing generated tests '{s}': {}\n", .{ tests_path, err });
                return err;
            };

            std.debug.print("Wrote generated tests to: {s}\n", .{tests_path});
        }

        // Generate deployment manifest if --deploy was passed
        if (deploy_target_str) |dt_str| {
            const deploy_target = deploy_manifest.DeployTarget.fromString(dt_str) orelse {
                std.debug.print("Unknown deploy target: {s} (supported: aws)\n", .{dt_str});
                return error.InvalidArgument;
            };

            const extract = deploy_manifest.extractProvenFacts(allocator, contract) catch |err| {
                std.debug.print("Error extracting proven facts: {}\n", .{err});
                return err;
            };
            defer allocator.free(extract.checks_buf);
            defer allocator.free(extract.routes_buf);

            const outputs = deploy_manifest.render(allocator, deploy_target, &extract.facts) catch |err| {
                std.debug.print("Error rendering deploy manifest: {}\n", .{err});
                return err;
            };
            defer {
                for (outputs) |o| allocator.free(o.content);
                allocator.free(outputs);
            }

            // Derive deploy output directory from the output path
            const deploy_dir = deriveSiblingPath(allocator, output_path_final, "deploy/") catch |err| {
                std.debug.print("Error deriving deploy dir: {}\n", .{err});
                return err;
            };
            defer allocator.free(deploy_dir);

            for (outputs) |output| {
                const deploy_path = std.fmt.allocPrint(allocator, "{s}{s}", .{ deploy_dir, output.filename }) catch |err| {
                    std.debug.print("Error creating deploy path: {}\n", .{err});
                    return err;
                };
                defer allocator.free(deploy_path);

                writeFilePosix(deploy_path, output.content, allocator) catch |err| {
                    std.debug.print("Error writing deploy file '{s}': {}\n", .{ deploy_path, err });
                    return err;
                };

                std.debug.print("Wrote deploy manifest to: {s}\n", .{deploy_path});
            }

            // Write deploy report
            const report_path = std.fmt.allocPrint(allocator, "{s}deploy-report.txt", .{deploy_dir}) catch |err| {
                std.debug.print("Error creating report path: {}\n", .{err});
                return err;
            };
            defer allocator.free(report_path);

            var report_output: std.ArrayList(u8) = .empty;
            defer report_output.deinit(allocator);
            var report_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &report_output);

            deploy_manifest.writeDeployReport(&report_aw.writer, &extract.facts, deploy_target) catch |err| {
                std.debug.print("Error generating deploy report: {}\n", .{err});
                return err;
            };
            report_output = report_aw.toArrayList();

            writeFilePosix(report_path, report_output.items, allocator) catch |err| {
                std.debug.print("Error writing deploy report '{s}': {}\n", .{ report_path, err });
                return err;
            };

            std.debug.print("Wrote deploy report to: {s}\n", .{report_path});
            std.debug.print("Deploy target: {s}, proof level: {s}\n", .{
                deploy_target.toString(),
                extract.facts.proof_level.toString(),
            });
        }

        // Run proven evolution pipeline if --prove was passed
        if (prove_spec) |spec| {
            // Parse spec format: "contract.json" or "contract.json:traces.jsonl"
            var old_contract_path: []const u8 = spec;
            var prove_trace_path: ?[]const u8 = null;

            if (std.mem.indexOf(u8, spec, ":")) |colon_pos| {
                old_contract_path = spec[0..colon_pos];
                if (colon_pos + 1 < spec.len) {
                    prove_trace_path = spec[colon_pos + 1 ..];
                }
            }

            // Read old contract JSON
            const old_contract_json = readFilePosix(allocator, old_contract_path, 10 * 1024 * 1024) catch |err| {
                std.debug.print("Error reading old contract '{s}': {}\n", .{ old_contract_path, err });
                return err;
            };
            defer allocator.free(old_contract_json);

            // Run replay against traces if provided
            var prove_replay: ?prove_upgrade.ReplaySummary = null;
            if (prove_trace_path) |trace_path| {
                const prove_trace_source = readFilePosix(allocator, trace_path, 100 * 1024 * 1024) catch |err| {
                    std.debug.print("Error reading trace file '{s}': {}\n", .{ trace_path, err });
                    return err;
                };
                defer allocator.free(prove_trace_source);

                const groups = zts.trace.parseTraceFile(allocator, prove_trace_source) catch |err| {
                    std.debug.print("Error parsing trace file '{s}': {}\n", .{ trace_path, err });
                    return err;
                };
                defer {
                    for (groups) |g| allocator.free(g.io_calls);
                    allocator.free(groups);
                }

                if (groups.len > 0) {
                    std.debug.print("Replaying {d} traces for proven evolution...\n", .{groups.len});
                    const replay_result = runBuildTimeReplay(allocator, source, handler_path_final, groups) catch |err| {
                        std.debug.print("Replay verification failed: {}\n", .{err});
                        return err;
                    };
                    prove_replay = .{
                        .total = replay_result.total,
                        .identical = replay_result.pass,
                        .status_changed = 0,
                        .body_changed = 0,
                        .diverged = replay_result.fail,
                    };
                }
            }

            // Run the prove pipeline
            var result = prove_upgrade.prove(
                allocator,
                old_contract_json,
                contract,
                prove_replay,
                handler_path_final,
            ) catch |err| {
                std.debug.print("Proven evolution failed: {}\n", .{err});
                return err;
            };
            defer result.deinit(allocator);

            // Write outputs
            const prove_dir = deriveSiblingPath(allocator, output_path_final, "") catch |err| {
                std.debug.print("Error deriving prove output dir: {}\n", .{err});
                return err;
            };
            defer allocator.free(prove_dir);

            prove_upgrade.writeProofOutputs(allocator, &result.certificate, prove_dir) catch |err| {
                std.debug.print("Error writing proof outputs: {}\n", .{err});
                return err;
            };

            std.debug.print("Proven evolution: {s}\n", .{result.certificate.classification.toString()});
        }
    }
}

// ============================================================================
// Build-Time Replay Verification
// ============================================================================

const BuildReplayResult = struct {
    total: u32,
    pass: u32,
    fail: u32,
};

/// Run replay verification at build time.
/// Creates a fresh zts context per trace for isolation (the interpreter mutates
/// context state during execution). This is O(N*parse) but acceptable at build
/// time since trace counts are typically small.
fn runBuildTimeReplay(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    groups: []const zts.trace.RequestTraceGroup,
) !BuildReplayResult {
    var pass: u32 = 0;
    var fail: u32 = 0;

    for (groups, 0..) |*group, idx| {
        const ok = replayOneBuildTime(allocator, handler_source, handler_filename, group) catch |err| {
            std.debug.print("  Trace #{d}: error - {}\n", .{ idx, err });
            fail += 1;
            continue;
        };
        if (ok) {
            pass += 1;
        } else {
            fail += 1;
        }
    }

    return .{ .total = pass + fail, .pass = pass, .fail = fail };
}

/// Result of executing a handler at build time.
const HandlerExecResult = struct {
    status: u16,
    body: []const u8,
    divergences: u32,
};

/// Execute a handler against a request with stubbed I/O at build time.
/// Shared by both replay verification and handler tests.
fn executeBuildTimeHandler(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    request: zts.trace.RequestTrace,
    io_calls: []const zts.trace.IoEntry,
) !HandlerExecResult {
    const ctx = try zts.createContext(allocator, .{ .nursery_size = 64 * 1024 });
    defer zts.destroyContext(ctx);
    try zts.builtins.initBuiltins(ctx);

    inline for (std.meta.fields(zts.modules.VirtualModule)) |field| {
        const module: zts.modules.VirtualModule = @enumFromInt(field.value);
        try zts.modules.registerVirtualModuleReplay(module, ctx, allocator);
    }

    var replay_state = zts.trace.ReplayState{
        .io_calls = io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    ctx.setModuleState(
        zts.trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &zts.trace.ReplayState.deinitOpaque,
    );
    defer ctx.module_state[zts.trace.REPLAY_STATE_SLOT] = null;

    var strings = zts.StringTable.init(allocator);
    defer strings.deinit();

    var source_to_parse: []const u8 = handler_source;
    var strip_result: ?zts.StripResult = null;
    defer if (strip_result) |*sr| sr.deinit();

    const is_ts = std.mem.endsWith(u8, handler_filename, ".ts");
    const is_tsx = std.mem.endsWith(u8, handler_filename, ".tsx");
    if (is_ts or is_tsx) {
        strip_result = zts.strip(allocator, handler_source, .{ .tsx_mode = is_tsx }) catch return error.StripFailed;
        source_to_parse = strip_result.?.code;
    }

    var p = zts.Parser.init(allocator, source_to_parse, &strings, &ctx.atoms);
    defer p.deinit();
    if (std.mem.endsWith(u8, handler_filename, ".jsx") or is_tsx) {
        p.enableJsx();
    }

    const bytecode_data = p.parse() catch return error.ParseFailed;

    const shapes = p.getShapes();
    if (shapes.len > 0) {
        try ctx.materializeShapes(shapes);
    }

    const func = zts.bytecode.FunctionBytecode{
        .header = .{},
        .name_atom = 0,
        .arg_count = 0,
        .local_count = p.max_local_count,
        .stack_size = 256,
        .flags = .{},
        .code = bytecode_data,
        .constants = p.constants.items,
        .source_map = null,
    };

    var interp = zts.Interpreter.init(ctx);
    _ = try interp.run(&func);

    const handler_val = ctx.getGlobal(zts.Atom.handler) orelse return error.NoHandler;
    if (!handler_val.isObject()) return error.NoHandler;
    const handler_obj = zts.JSObject.fromValue(handler_val);
    if (handler_obj.class_id != .function or !handler_obj.flags.is_callable) return error.NoHandler;

    const hc_pool_req = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const req_obj = try ctx.createObject(null);

    const method_val = try ctx.createString(request.method);
    try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.method, method_val);

    const url_val = try ctx.createString(request.url);
    try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.url, url_val);

    if (request.body) |body| {
        const body_val = try ctx.createString(body);
        try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.body, body_val);
    }

    const args = [_]zts.JSValue{req_obj.toValue()};
    const bc_data = handler_obj.getBytecodeFunctionData() orelse return error.NotCallable;
    const result = interp.callBytecodeFunction(
        handler_obj.toValue(),
        bc_data.bytecode,
        zts.JSValue.undefined_val,
        &args,
    ) catch return error.HandlerError;

    if (!result.isObject()) return error.InvalidResponse;
    const result_obj = zts.JSObject.fromValue(result);
    const hc_pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const status_val = result_obj.getProperty(hc_pool, zts.Atom.status) orelse zts.JSValue.fromInt(200);
    const actual_status: u16 = if (status_val.isInt())
        @intCast(@max(0, @min(999, status_val.getInt())))
    else
        200;

    const body_val = result_obj.getProperty(hc_pool, zts.Atom.body) orelse zts.JSValue.undefined_val;

    return .{
        .status = actual_status,
        .body = zts.trace.extractStringData(body_val) orelse "",
        .divergences = replay_state.divergences,
    };
}

/// Replay a single trace at build time.
fn replayOneBuildTime(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    group: *const zts.trace.RequestTraceGroup,
) !bool {
    const r = try executeBuildTimeHandler(allocator, handler_source, handler_filename, group.request, group.io_calls);

    const expected = group.response orelse return true;
    const status_ok = r.status == expected.status;

    // Unescape before comparing (trace stores JSON-escaped body strings).
    const expected_body = zts.trace.unescapeJson(allocator, expected.body) catch expected.body;
    defer if (expected_body.ptr != expected.body.ptr) allocator.free(expected_body);

    return status_ok and std.mem.eql(u8, r.body, expected_body) and r.divergences == 0;
}

// ============================================================================
// Build-Time Handler Tests
// ============================================================================

const BuildTestCase = struct {
    name: []const u8,
    request: ?zts.trace.RequestTrace = null,
    io_calls: []const zts.trace.IoEntry = &.{},
    expected_status: ?u16 = null,
    expected_body: ?[]const u8 = null,
    expected_body_contains: ?[]const u8 = null,
};

fn runBuildTimeTests(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    test_source: []const u8,
) !BuildReplayResult {
    const tests = try parseBuildTestFile(allocator, test_source);
    defer {
        for (tests) |t| allocator.free(t.io_calls);
        allocator.free(tests);
    }

    var pass: u32 = 0;
    var fail: u32 = 0;

    for (tests) |*tc| {
        const request = tc.request orelse {
            std.debug.print("  FAIL  {s} (no request defined)\n", .{tc.name});
            fail += 1;
            continue;
        };

        const r = executeBuildTimeHandler(allocator, handler_source, handler_filename, request, tc.io_calls) catch |err| {
            std.debug.print("  FAIL  {s} (error: {})\n", .{ tc.name, err });
            fail += 1;
            continue;
        };

        var ok = true;
        if (tc.expected_status) |expected_status| {
            if (r.status != expected_status) {
                std.debug.print("  FAIL  {s}\n        expected status: {d}, actual status: {d}\n", .{
                    tc.name, expected_status, r.status,
                });
                ok = false;
            }
        }
        if (tc.expected_body) |expected| {
            const unescaped = zts.trace.unescapeJson(allocator, expected) catch expected;
            defer if (unescaped.ptr != expected.ptr) allocator.free(unescaped);
            if (!std.mem.eql(u8, r.body, unescaped)) {
                std.debug.print("  FAIL  {s}\n        body mismatch\n        expected: {s}\n        actual:   {s}\n", .{
                    tc.name, truncate(unescaped, 200), truncate(r.body, 200),
                });
                ok = false;
            }
        }
        if (tc.expected_body_contains) |needle| {
            const unescaped = zts.trace.unescapeJson(allocator, needle) catch needle;
            defer if (unescaped.ptr != needle.ptr) allocator.free(unescaped);
            if (std.mem.indexOf(u8, r.body, unescaped) == null) {
                std.debug.print("  FAIL  {s}\n        body does not contain: {s}\n", .{ tc.name, unescaped });
                ok = false;
            }
        }

        if (ok) {
            std.debug.print("  PASS  {s}\n", .{tc.name});
            pass += 1;
        } else {
            fail += 1;
        }
    }

    return .{ .total = pass + fail, .pass = pass, .fail = fail };
}

fn truncate(s: []const u8, max: usize) []const u8 {
    return if (s.len > max) s[0..max] else s;
}

fn parseBuildTestFile(allocator: std.mem.Allocator, source: []const u8) ![]BuildTestCase {
    var tests: std.ArrayList(BuildTestCase) = .empty;
    errdefer tests.deinit(allocator);

    var current_name: ?[]const u8 = null;
    var current_request: ?zts.trace.RequestTrace = null;
    var current_io: std.ArrayList(zts.trace.IoEntry) = .empty;
    defer current_io.deinit(allocator);
    var current_status: ?u16 = null;
    var current_body: ?[]const u8 = null;
    var current_body_contains: ?[]const u8 = null;

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const type_str = zts.trace.findJsonStringValue(line, "\"type\"") orelse continue;

        if (std.mem.eql(u8, type_str, "test")) {
            if (current_name != null) {
                try tests.append(allocator, .{
                    .name = current_name.?,
                    .request = current_request,
                    .io_calls = try current_io.toOwnedSlice(allocator),
                    .expected_status = current_status,
                    .expected_body = current_body,
                    .expected_body_contains = current_body_contains,
                });
                current_request = null;
                current_status = null;
                current_body = null;
                current_body_contains = null;
            }
            current_name = zts.trace.findJsonStringValue(line, "\"name\"") orelse "unnamed";
        } else if (std.mem.eql(u8, type_str, "request")) {
            current_request = .{
                .method = zts.trace.findJsonStringValue(line, "\"method\"") orelse "GET",
                .url = zts.trace.findJsonStringValue(line, "\"url\"") orelse "/",
                .headers_json = zts.trace.findJsonObjectValue(line, "\"headers\"") orelse "{}",
                .body = zts.trace.findJsonStringValue(line, "\"body\""),
            };
        } else if (std.mem.eql(u8, type_str, "io")) {
            try current_io.append(allocator, .{
                .seq = @intCast(zts.trace.findJsonIntValue(line, "\"seq\"") orelse 0),
                .module = zts.trace.findJsonStringValue(line, "\"module\"") orelse "",
                .func = zts.trace.findJsonStringValue(line, "\"fn\"") orelse "",
                .args_json = zts.trace.findJsonArrayValue(line, "\"args\"") orelse "[]",
                .result_json = zts.trace.findJsonAnyValue(line, "\"result\"") orelse "null",
            });
        } else if (std.mem.eql(u8, type_str, "expect")) {
            const status_val = zts.trace.findJsonIntValue(line, "\"status\"");
            current_status = if (status_val) |s| @intCast(@max(0, @min(999, s))) else null;
            current_body = zts.trace.findJsonStringValue(line, "\"body\"");
            current_body_contains = zts.trace.findJsonStringValue(line, "\"bodyContains\"");
        }
    }

    if (current_name != null) {
        try tests.append(allocator, .{
            .name = current_name.?,
            .request = current_request,
            .io_calls = try current_io.toOwnedSlice(allocator),
            .expected_status = current_status,
            .expected_body = current_body,
            .expected_body_contains = current_body_contains,
        });
    }

    return try tests.toOwnedSlice(allocator);
}

fn compileHandler(
    allocator: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    emit_aot: bool,
    emit_verify: bool,
    emit_contract: bool,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
    generate_tests: bool,
) !CompiledHandler {
    var source_to_parse: []const u8 = source;
    var strip_result: ?zts.StripResult = null;
    defer if (strip_result) |*sr| sr.deinit();

    // Type strip for .ts/.tsx files
    const is_ts = std.mem.endsWith(u8, filename, ".ts");
    const is_tsx = std.mem.endsWith(u8, filename, ".tsx");

    if (is_ts or is_tsx) {
        // Build comptime environment with build metadata
        const comptime_env = zts.ComptimeEnv{
            .build_time = null, // TODO: pass actual build time
            .git_commit = null, // TODO: pass git commit
            .version = zts.version.string,
            .env_vars = null,
        };

        strip_result = zts.strip(allocator, source, .{
            .tsx_mode = is_tsx,
            .enable_comptime = true,
            .comptime_env = comptime_env,
        }) catch |err| {
            std.debug.print("TypeScript strip error: {}\n", .{err});
            return err;
        };
        source_to_parse = strip_result.?.code;
        std.debug.print("TypeScript stripped successfully\n", .{});
    }

    // Initialize string table and atom table for parsing
    var strings = zts.StringTable.init(allocator);
    defer strings.deinit();

    var atoms = zts.context.AtomTable.init(allocator);
    defer atoms.deinit();

    // Parse the source code (single pass for IR + bytecode)
    var js_parser = zts.parser.JsParser.init(allocator, source_to_parse);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    // Enable JSX mode for .jsx and .tsx files
    if (std.mem.endsWith(u8, filename, ".jsx") or is_tsx) {
        js_parser.tokenizer.enableJsx();
    }

    const root = js_parser.parse() catch |err| {
        // Print parse errors
        const errors = js_parser.errors.getErrors();
        if (errors.len > 0) {
            for (errors) |parse_error| {
                std.debug.print("Parse error at {s}:{}:{}: {s}\n", .{
                    filename,
                    parse_error.location.line,
                    parse_error.location.column,
                    parse_error.message,
                });
            }
        }
        return err;
    };

    const needs_contract = true; // Always extract for auto-sandboxing

    // Check for file imports before proceeding with single-module compilation
    const has_file_imports = hasFileImports(&js_parser, root);

    if (has_file_imports) {
        if (!builtin.is_test) std.debug.print("File imports detected, building module graph...\n", .{});
        return compileMultiModule(
            allocator,
            source_to_parse,
            filename,
            &strings,
            &atoms,
            needs_contract,
            emit_contract,
            policy,
            sql_schema_path,
        );
    }

    // Optimize IR (cold-start-friendly single pass)
    _ = zts.parser.optimizeIR(
        allocator,
        &js_parser.nodes,
        &js_parser.constants,
        root,
    ) catch {};

    // Node type map for type-directed codegen (populated by BoolChecker, consumed by CodeGen)
    var node_type_map: zts.bool_checker.NodeTypeMap = .empty;
    defer node_type_map.deinit(allocator);

    // Run bool checker and type checker
    {
        const ir_view_check = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        var checker = zts.BoolChecker.init(allocator, ir_view_check, &atoms);
        defer checker.deinit();

        const bool_error_count = try checker.check(root);
        const bool_diags = checker.getDiagnostics();

        if (bool_diags.len > 0) {
            std.debug.print("\n", .{});
            var bool_output: std.ArrayList(u8) = .empty;
            defer bool_output.deinit(allocator);
            var bool_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &bool_output);
            checker.formatDiagnostics(source_to_parse, &bool_aw.writer) catch {};
            bool_output = bool_aw.toArrayList();
            if (bool_output.items.len > 0) {
                std.debug.print("{s}", .{bool_output.items});
            }
            std.debug.print("{d} boolean check error(s), {d} warning(s)\n", .{
                bool_error_count,
                bool_diags.len - bool_error_count,
            });
        }

        if (bool_error_count > 0) {
            std.debug.print("\nBoolean check failed for {s}\n", .{filename});
            return error.SoundModeViolation;
        }

        std.debug.print("Boolean check passed\n", .{});

        // Extract node type map for codegen specialization (move ownership out of checker)
        node_type_map = checker.node_types;
        checker.node_types = .empty; // Prevent double-free in checker.deinit()

        // TypeChecker: full type annotation checking (when TypeMap available from .ts/.tsx)
        if (strip_result) |sr| {
            const tm = sr.type_map;
            var type_pool = zts.TypePool.init(allocator);
            defer type_pool.deinit(allocator);

            var type_env = zts.TypeEnv.init(allocator, &type_pool);
            defer type_env.deinit();

            zts.modules.populateModuleTypes(&type_env, &type_pool, allocator);
            type_env.populateFromTypeMap(&tm);

            var tc = zts.type_checker.TypeChecker.init(allocator, ir_view_check, &atoms, &type_env);
            defer tc.deinit();

            const tc_errors = try tc.check(root);
            const tc_diags = tc.getDiagnostics();

            if (tc_diags.len > 0) {
                var tc_output: std.ArrayList(u8) = .empty;
                defer tc_output.deinit(allocator);
                var tc_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &tc_output);
                tc.formatDiagnostics(source_to_parse, &tc_aw.writer) catch {};
                tc_output = tc_aw.toArrayList();
                if (tc_output.items.len > 0) {
                    std.debug.print("{s}", .{tc_output.items});
                }
            }

            if (tc_errors > 0) {
                std.debug.print("\nType check failed for {s}\n", .{filename});
                return error.SoundModeViolation;
            }
            std.debug.print("Type check passed\n", .{});
        }
    }

    // Run handler verification if requested
    var verify_info: ?VerificationInfo = null;
    if (emit_verify) {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = zts.handler_verifier.findHandlerFunction(ir_view, root);

        if (handler_fn) |hf| {
            var verifier = zts.HandlerVerifier.init(allocator, ir_view, &atoms);
            defer verifier.deinit();

            const error_count = try verifier.verify(hf);
            const diags = verifier.getDiagnostics();

            if (diags.len > 0) {
                std.debug.print("\n", .{});
                // Format diagnostics to a buffer and print via debug.print
                var diag_output: std.ArrayList(u8) = .empty;
                defer diag_output.deinit(allocator);
                var diag_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &diag_output);
                verifier.formatDiagnostics(source_to_parse, &diag_aw.writer) catch {};
                diag_output = diag_aw.toArrayList();
                if (diag_output.items.len > 0) {
                    std.debug.print("{s}", .{diag_output.items});
                }
                std.debug.print("{d} error(s), {d} warning(s)\n", .{
                    error_count,
                    diags.len - error_count,
                });
            }

            if (error_count > 0) {
                std.debug.print("\nVerification failed for {s}\n", .{filename});
                return error.VerificationFailed;
            }

            // Track verification results for contract
            var has_unreachable = false;
            for (diags) |d| {
                if (d.kind == .unreachable_after_return) {
                    has_unreachable = true;
                    break;
                }
            }
            verify_info = .{
                .exhaustive_returns = true,
                .results_safe = error_count == 0,
                .unreachable_code = has_unreachable,
                .bytecode_verified = true, // will be set after bytecode gen
            };

            std.debug.print("Verification passed\n", .{});
        } else {
            std.debug.print("Warning: no handler function found for verification\n", .{});
        }
    }

    var code_gen = zts.parser.CodeGen.initWithIRStore(
        allocator,
        &js_parser.nodes,
        &js_parser.constants,
        &js_parser.scopes,
        &strings,
        &atoms,
    );
    defer code_gen.deinit();

    // Wire type annotations from BoolChecker for type-directed opcode specialization
    if (node_type_map.count() > 0) {
        code_gen.setNodeTypes(&node_type_map);
    }

    const func = try code_gen.generate(root);
    defer code_gen.freeOwnedConstantPayloads();

    std.debug.print("Parsed successfully: {d} bytes of bytecode\n", .{func.code.len});

    // Bytecode verification: reject malformed bytecode before serialization
    const verify_bc = zts.BytecodeVerifier.verify(&func);
    if (!verify_bc.valid) {
        std.debug.print("Bytecode verification failed at offset {d}: {s}\n", .{
            verify_bc.offset,
            verify_bc.message,
        });
        return error.BytecodeVerificationFailed;
    }

    // Get object literal shapes from parser
    const shapes = code_gen.shapes.items;
    std.debug.print("Collected {d} object literal shapes\n", .{shapes.len});

    // Serialize bytecode with atoms AND shapes for complete cache format
    var buffer: [256 * 1024]u8 = undefined; // 256KB buffer
    var writer = zts.bytecode_cache.SliceWriter{ .buffer = &buffer };

    zts.bytecode_cache.serializeBytecodeWithAtomsAndShapes(&func, &atoms, shapes, &writer, allocator) catch |err| {
        std.debug.print("Serialization error: {}\n", .{err});
        return err;
    };

    // Copy the serialized data to owned memory
    const serialized = writer.getWritten();
    const bytecode_data = try allocator.dupe(u8, serialized);

    var aot: ?AotAnalysis = null;
    var transpiled_source: ?[]const u8 = null;

    if (emit_aot) {
        // Try transpiler first (general-purpose IR-to-Zig)
        const ir_view_aot = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        var transpiler = IrTranspiler.init(allocator, ir_view_aot, &atoms);
        // Note: transpiler is NOT deferred deinit - its output is borrowed

        const result = transpiler.transpileHandler(root) catch |err| {
            std.debug.print("Transpiler error: {}, falling back to pattern matcher\n", .{err});
            transpiler.deinit();
            aot = try analyzeAot(allocator, &js_parser, &atoms, root);

            // Build contract if requested or needed for policy validation.
            const contract = if (needs_contract)
                try buildContractWithPolicy(
                    allocator,
                    &js_parser,
                    &atoms,
                    filename,
                    root,
                    aot,
                    verify_info,
                    if (strip_result) |*sr| &sr.type_map else null,
                    policy,
                    sql_schema_path,
                )
            else
                null;

            return .{
                .bytecode = bytecode_data,
                .aot = aot,
                .contract = contract,
            };
        };

        if (result.handler_name != null and result.functions_transpiled > 0) {
            std.debug.print("Transpiler: {d} functions transpiled, {d} bailed\n", .{
                result.functions_transpiled, result.functions_bailed,
            });
            transpiled_source = try allocator.dupe(u8, result.source);
            transpiler.deinit();
        } else {
            std.debug.print("Transpiler produced no output, falling back to pattern matcher\n", .{});
            transpiler.deinit();
            aot = try analyzeAot(allocator, &js_parser, &atoms, root);
        }
    }

    // Build contract if requested or needed for policy validation.
    // When contract output or policy validation is enabled, always run AOT
    // analysis for route extraction even if the transpiler succeeded.
    // even if the transpiler succeeded (transpiler doesn't produce a dispatch table).
    var contract: ?HandlerContract = null;
    if (needs_contract) {
        // Run AOT analysis for route extraction if not already done
        var temp_aot = if (aot == null) try analyzeAot(allocator, &js_parser, &atoms, root) else null;
        defer if (temp_aot) |*t| t.deinit(allocator);
        const effective_aot = aot orelse temp_aot;

        contract = try buildContractWithPolicy(
            allocator,
            &js_parser,
            &atoms,
            filename,
            root,
            effective_aot,
            verify_info,
            if (strip_result) |*sr| &sr.type_map else null,
            policy,
            sql_schema_path,
        );
    }

    // Generate exhaustive test cases from path analysis
    var generated_tests_jsonl: ?[]const u8 = null;
    if (generate_tests) {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = findHandlerFunction(ir_view, root);
        if (handler_fn) |hf| {
            var gen = zts.PathGenerator.init(allocator, ir_view, &atoms);
            defer gen.deinit();

            try gen.generate(hf);

            const test_count = gen.getTests().len;
            if (test_count > 0) {
                var jsonl_buf: std.ArrayList(u8) = .empty;
                var jsonl_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &jsonl_buf);
                gen.writeJsonl(&jsonl_aw.writer) catch {};
                jsonl_buf = jsonl_aw.toArrayList();
                generated_tests_jsonl = try jsonl_buf.toOwnedSlice(allocator);
            }

            // Compute max I/O depth across all generated paths
            if (contract != null) {
                const tests = gen.getTests();
                if (contract.?.properties) |*props| {
                    var max_depth: u32 = 0;
                    for (tests) |t| {
                        const depth: u32 = @intCast(t.io_stubs.items.len);
                        if (depth > max_depth) max_depth = depth;
                    }
                    props.max_io_depth = if (tests.len > 0) max_depth else null;
                }
            }

            // Fault coverage analysis on generated paths
            var fc = zts.fault_coverage.FaultCoverageChecker.init(allocator, gen.getTests());
            defer fc.deinit();
            try fc.analyze();
            const fc_report = fc.getReport();

            if (fc_report.total_failable > 0) {
                if (contract != null) {
                    contract.?.fault_coverage = .{
                        .total_failable = fc_report.total_failable,
                        .covered = fc_report.covered,
                        .warnings = fc_report.warning_count,
                    };
                    if (contract.?.properties) |*props| {
                        props.fault_covered = fc_report.isClean();
                    }
                }

                if (!builtin.is_test) {
                    var fc_buf: std.ArrayList(u8) = .empty;
                    var fc_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &fc_buf);
                    fc.formatMatrix(fc_report, &fc_aw.writer) catch {};
                    fc_buf = fc_aw.toArrayList();
                    if (fc_buf.items.len > 0) {
                        std.debug.print("{s}", .{fc_buf.items});
                    }
                    fc_buf.deinit(allocator);

                    if (fc_report.warning_count > 0) {
                        var diag_buf: std.ArrayList(u8) = .empty;
                        var diag_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &diag_buf);
                        fc.formatDiagnostics(&diag_aw.writer) catch {};
                        diag_buf = diag_aw.toArrayList();
                        if (diag_buf.items.len > 0) {
                            std.debug.print("{s}", .{diag_buf.items});
                        }
                        diag_buf.deinit(allocator);
                    }
                }
            }

            if (!builtin.is_test) {
                std.debug.print("Generated {d} test case(s) from path analysis\n", .{test_count});
            }
        }
    }

    return .{
        .bytecode = bytecode_data,
        .aot = aot,
        .transpiled_source = transpiled_source,
        .contract = contract,
        .generated_tests = generated_tests_jsonl,
    };
}

/// Scan parsed IR for file import declarations
fn hasFileImports(js_parser: *zts.parser.JsParser, _: ir.NodeIndex) bool {
    const view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const node_count = view.nodeCount();

    for (0..node_count) |idx| {
        const tag = view.getTag(@intCast(idx)) orelse continue;
        if (tag != .import_decl) continue;

        const import_decl = view.getImportDecl(@intCast(idx)) orelse continue;
        const module_str = view.getString(import_decl.module_idx) orelse continue;

        const result = zts.modules.resolver.resolve(module_str);
        switch (result) {
            .file => return true,
            .virtual, .unknown => {},
        }
    }

    return false;
}

/// Compile a handler with file imports as a multi-module bundle
fn compileMultiModule(
    allocator: std.mem.Allocator,
    entry_source: []const u8,
    filename: []const u8,
    strings: *zts.string.StringTable,
    atoms: *zts.context.AtomTable,
    needs_contract: bool,
    emit_contract: bool,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
) !CompiledHandler {
    // Build module graph
    var graph = zts.modules.ModuleGraph.init(allocator);
    defer graph.deinit();

    graph.build(filename, entry_source, readFilePosixForGraph) catch |err| {
        if (!builtin.is_test) std.debug.print("Module graph error: {}\n", .{err});
        return err;
    };

    if (!builtin.is_test) std.debug.print("Module graph: {d} modules ({d} dependencies)\n", .{
        graph.module_list.items.len,
        graph.dependencyCount(),
    });

    // Compile all modules with shared atoms
    var module_compiler = zts.modules.ModuleCompiler.init(allocator, atoms, strings);
    var compile_result = module_compiler.compileAll(&graph) catch |err| {
        std.debug.print("Multi-module compilation error: {}\n", .{err});
        return err;
    };
    defer compile_result.deinit();
    defer {
        for (compile_result.codegens) |*cg| cg.freeOwnedConstantPayloads();
    }

    // Serialize each module and collect dependency bytecodes
    const mod_count = compile_result.modules.len;
    var dep_bytecodes = try allocator.alloc([]const u8, mod_count - 1);
    errdefer {
        for (dep_bytecodes) |d| allocator.free(d);
        allocator.free(dep_bytecodes);
    }

    // Serialize dependency modules (all except last, which is the entry)
    for (compile_result.modules[0 .. mod_count - 1], 0..) |*compiled_mod, i| {
        var buffer: [256 * 1024]u8 = undefined;
        var writer = zts.bytecode_cache.SliceWriter{ .buffer = &buffer };

        zts.bytecode_cache.serializeBytecodeWithAtomsAndShapes(
            &compiled_mod.func,
            atoms,
            compiled_mod.shapes,
            &writer,
            allocator,
        ) catch |err| {
            std.debug.print("Dependency serialization error: {}\n", .{err});
            return err;
        };

        dep_bytecodes[i] = try allocator.dupe(u8, writer.getWritten());
    }

    // Serialize entry module (last in execution order)
    const entry_mod = &compile_result.modules[mod_count - 1];
    var entry_buffer: [256 * 1024]u8 = undefined;
    var entry_writer = zts.bytecode_cache.SliceWriter{ .buffer = &entry_buffer };

    zts.bytecode_cache.serializeBytecodeWithAtomsAndShapes(
        &entry_mod.func,
        atoms,
        entry_mod.shapes,
        &entry_writer,
        allocator,
    ) catch |err| {
        std.debug.print("Entry serialization error: {}\n", .{err});
        return err;
    };

    const entry_bytecode = try allocator.dupe(u8, entry_writer.getWritten());
    errdefer allocator.free(entry_bytecode);

    if (!builtin.is_test) std.debug.print("Multi-module bundle: {d} dependency modules + entry\n", .{dep_bytecodes.len});

    var contract: ?HandlerContract = null;
    if (needs_contract) {
        contract = try buildMultiModuleContract(
            allocator,
            &graph,
            &compile_result,
            atoms,
            filename,
            emit_contract,
            policy,
            sql_schema_path,
        );
    }

    return .{
        .bytecode = entry_bytecode,
        .dep_bytecodes = dep_bytecodes,
        .contract = contract,
    };
}

const readFilePosixForGraph = zts.file_io.readFileForModuleGraph;

fn analyzeAot(
    allocator: std.mem.Allocator,
    js_parser: *zts.parser.JsParser,
    atoms: *zts.context.AtomTable,
    root: ir.NodeIndex,
) !?AotAnalysis {
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const handler_fn = findHandlerFunction(ir_view, root) orelse return null;

    var analyzer = zts.HandlerAnalyzer.init(allocator, ir_view, atoms);
    defer analyzer.deinit();
    analyzer.enableJsonBodyParsePatterns();

    var dispatch = try analyzer.analyze(handler_fn);
    const default_response = try analyzer.analyzeDirectReturn(handler_fn);

    if (dispatch) |d| {
        const emittable = countAotPatterns(d);
        if (emittable == 0) {
            d.deinit();
            allocator.destroy(d);
            dispatch = null;
        }
    }

    if (dispatch == null and default_response == null) {
        return null;
    }

    var handler_loc: ?zts.parser.SourceLocation = null;
    if (handler_fn < js_parser.nodes.locs.items.len) {
        handler_loc = js_parser.nodes.locs.items[handler_fn];
    }

    return .{
        .dispatch = dispatch,
        .default_response = default_response,
        .handler_loc = handler_loc,
    };
}

fn countAotPatterns(dispatch: *const zts.PatternDispatchTable) usize {
    var count: usize = 0;
    for (dispatch.patterns) |pattern| {
        switch (pattern.pattern_type) {
            .exact => count += 1,
            .prefix => {
                if (pattern.response_template_prefix != null) count += 1;
            },
            else => {},
        }
    }
    return count;
}

// findHandlerFunction is defined in handler_verifier.zig and exported via zts.
const findHandlerFunction = zts.handler_verifier.findHandlerFunction;

/// Build a contract manifest from the parsed IR.
fn buildContract(
    allocator: std.mem.Allocator,
    js_parser: *zts.parser.JsParser,
    atoms: *zts.context.AtomTable,
    filename: []const u8,
    root: ir.NodeIndex,
    aot: ?AotAnalysis,
    verify_info: ?VerificationInfo,
    type_map: ?*const zts.TypeMap,
) !HandlerContract {
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);

    // Get handler location
    var handler_loc: ?ir.SourceLocation = null;
    const handler_fn = findHandlerFunction(ir_view, root);
    if (handler_fn) |hf| {
        if (hf < js_parser.nodes.locs.items.len) {
            handler_loc = js_parser.nodes.locs.items[hf];
        }
    }

    var type_pool = zts.TypePool.init(allocator);
    defer type_pool.deinit(allocator);

    var type_env = zts.TypeEnv.init(allocator, &type_pool);
    defer type_env.deinit();
    zts.modules.populateModuleTypes(&type_env, &type_pool, allocator);
    if (type_map) |tm| {
        type_env.populateFromTypeMap(tm);
    }

    var type_checker = zts.TypeChecker.init(allocator, ir_view, atoms, &type_env);
    defer type_checker.deinit();
    _ = type_checker.check(root) catch 0;

    var builder = ContractBuilder.init(allocator, ir_view, atoms, &type_env, &type_checker);
    defer builder.deinit();

    const dispatch = if (aot) |a| a.dispatch else null;
    const has_default = if (aot) |a| a.default_response != null else false;

    return builder.build(
        filename,
        handler_loc,
        dispatch,
        has_default,
        verify_info,
    );
}

fn buildContractWithPolicy(
    allocator: std.mem.Allocator,
    js_parser: *zts.parser.JsParser,
    atoms: *zts.context.AtomTable,
    filename: []const u8,
    root: ir.NodeIndex,
    aot: ?AotAnalysis,
    verify_info: ?VerificationInfo,
    type_map: ?*const zts.TypeMap,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
) !HandlerContract {
    var contract = try buildContract(
        allocator,
        js_parser,
        atoms,
        filename,
        root,
        aot,
        verify_info,
        type_map,
    );
    errdefer contract.deinit(allocator);

    // Run data flow provenance analysis
    {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = findHandlerFunction(ir_view, root);
        if (handler_fn) |hf| {
            var flow = zts.FlowChecker.init(allocator, ir_view, atoms);
            defer flow.deinit();

            const flow_errors = try flow.check(hf);
            const flow_diags = flow.getDiagnostics();

            if (flow_diags.len > 0) {
                if (!builtin.is_test) {
                    std.debug.print("\n", .{});
                    var flow_output: std.ArrayList(u8) = .empty;
                    defer flow_output.deinit(allocator);
                    var flow_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &flow_output);
                    flow.formatDiagnostics("", &flow_aw.writer) catch {};
                    flow_output = flow_aw.toArrayList();
                    if (flow_output.items.len > 0) {
                        std.debug.print("{s}", .{flow_output.items});
                    }
                    std.debug.print("{d} flow error(s), {d} warning(s)\n", .{
                        flow_errors,
                        flow_diags.len - flow_errors,
                    });
                }
            }

            // Inject flow properties into contract
            const flow_props = flow.getProperties();
            if (contract.properties) |*props| {
                props.no_secret_leakage = flow_props.no_secret_leakage;
                props.no_credential_leakage = flow_props.no_credential_leakage;
                props.input_validated = flow_props.input_validated;
                props.pii_contained = flow_props.pii_contained;
            }

            if (!builtin.is_test and flow_errors == 0) {
                std.debug.print("Flow analysis passed\n", .{});
            }
        }
    }

    try validateSqlContract(allocator, &contract, sql_schema_path);
    try enforcePolicyForContract(allocator, filename, &contract, policy);
    if (policy != null) {
        if (!builtin.is_test) std.debug.print("Capability policy check passed\n", .{});
    }

    return contract;
}

fn buildMultiModuleContract(
    allocator: std.mem.Allocator,
    graph: *const zts.modules.ModuleGraph,
    compile_result: *const zts.modules.CompileResult,
    atoms: *zts.context.AtomTable,
    entry_filename: []const u8,
    emit_contract: bool,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
) !HandlerContract {
    var merged = try initMergedContract(allocator, entry_filename);
    errdefer merged.deinit(allocator);

    const entry_index = compile_result.modules.len - 1;
    var policy_checked = false;

    for (compile_result.modules, compile_result.parsers, 0..) |compiled_module, *js_parser, idx| {
        const graph_idx = graph.execution_order[idx];
        const module = &graph.module_list.items[graph_idx];
        const is_entry = idx == entry_index;

        var temp_aot = if (is_entry and emit_contract)
            try analyzeAot(allocator, js_parser, atoms, compiled_module.root)
        else
            null;
        defer if (temp_aot) |*aot| aot.deinit(allocator);

        var module_contract = try buildContract(
            allocator,
            js_parser,
            atoms,
            module.path,
            compiled_module.root,
            temp_aot,
            null,
            null,
        );
        defer module_contract.deinit(allocator);

        try validateSqlContract(allocator, &module_contract, sql_schema_path);
        try enforcePolicyForContract(allocator, module.path, &module_contract, policy);
        if (policy != null) policy_checked = true;

        try mergeModuleContract(allocator, &merged, &module_contract, is_entry);
    }

    if (policy_checked) {
        if (!builtin.is_test) std.debug.print("Capability policy check passed\n", .{});
    }

    return merged;
}

fn initMergedContract(allocator: std.mem.Allocator, handler_path: []const u8) !HandlerContract {
    return .{
        .handler = .{
            .path = try allocator.dupe(u8, handler_path),
            .line = 0,
            .column = 0,
        },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = .{ .backend = "sqlite", .queries = .empty, .dynamic = false },
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .api = .{
            .schemas = .empty,
            .requests = .{ .schema_refs = .empty, .dynamic = false },
            .auth = .{ .bearer = false, .jwt = false },
            .routes = .empty,
            .schemas_dynamic = false,
            .routes_dynamic = false,
        },
        .verification = null,
        .aot = null,
    };
}

fn mergeModuleContract(
    allocator: std.mem.Allocator,
    target: *HandlerContract,
    source: *const HandlerContract,
    is_entry: bool,
) !void {
    if (is_entry) {
        target.handler.line = source.handler.line;
        target.handler.column = source.handler.column;
        target.verification = source.verification;
        target.aot = source.aot;

        for (source.routes.items) |route| {
            try target.routes.append(allocator, .{
                .pattern = try allocator.dupe(u8, route.pattern),
                .route_type = route.route_type,
                .field = route.field,
                .status = route.status,
                .content_type = route.content_type,
                .aot = route.aot,
            });
        }
    }

    for (source.modules.items) |name| {
        try appendUniqueString(allocator, &target.modules, name, false);
    }

    for (source.functions.items) |entry| {
        const target_entry = try getOrCreateFunctionEntry(allocator, &target.functions, entry.module);
        for (entry.names.items) |name| {
            try appendUniqueString(allocator, &target_entry.names, name, false);
        }
    }

    for (source.env.literal.items) |name| {
        try appendUniqueString(allocator, &target.env.literal, name, false);
    }
    target.env.dynamic = target.env.dynamic or source.env.dynamic;

    for (source.egress.hosts.items) |host| {
        try appendUniqueString(allocator, &target.egress.hosts, host, true);
    }
    target.egress.dynamic = target.egress.dynamic or source.egress.dynamic;

    for (source.cache.namespaces.items) |ns| {
        try appendUniqueString(allocator, &target.cache.namespaces, ns, false);
    }
    target.cache.dynamic = target.cache.dynamic or source.cache.dynamic;

    for (source.sql.queries.items) |query| {
        try appendSqlQuery(allocator, &target.sql.queries, query);
    }
    target.sql.dynamic = target.sql.dynamic or source.sql.dynamic;

    target.durable.used = target.durable.used or source.durable.used;
    for (source.durable.keys.literal.items) |key| {
        try appendUniqueString(allocator, &target.durable.keys.literal, key, false);
    }
    target.durable.keys.dynamic = target.durable.keys.dynamic or source.durable.keys.dynamic;
    for (source.durable.steps.items) |step| {
        try appendUniqueString(allocator, &target.durable.steps, step, false);
    }
    target.durable.timers = target.durable.timers or source.durable.timers;
    for (source.durable.signals.literal.items) |signal| {
        try appendUniqueString(allocator, &target.durable.signals.literal, signal, false);
    }
    target.durable.signals.dynamic = target.durable.signals.dynamic or source.durable.signals.dynamic;
    for (source.durable.producer_keys.literal.items) |key| {
        try appendUniqueString(allocator, &target.durable.producer_keys.literal, key, false);
    }
    target.durable.producer_keys.dynamic = target.durable.producer_keys.dynamic or source.durable.producer_keys.dynamic;

    for (source.api.schemas.items) |schema| {
        try upsertApiSchema(allocator, &target.api.schemas, schema.name, schema.schema_json);
    }

    for (source.api.requests.schema_refs.items) |schema_ref| {
        try appendUniqueString(allocator, &target.api.requests.schema_refs, schema_ref, false);
    }
    target.api.requests.dynamic = target.api.requests.dynamic or source.api.requests.dynamic;
    target.api.auth.bearer = target.api.auth.bearer or source.api.auth.bearer;
    target.api.auth.jwt = target.api.auth.jwt or source.api.auth.jwt;
    target.api.schemas_dynamic = target.api.schemas_dynamic or source.api.schemas_dynamic;
    target.api.routes_dynamic = target.api.routes_dynamic or source.api.routes_dynamic;

    for (source.api.routes.items) |route| {
        if (hasApiRoute(target.api.routes.items, route.method, route.path)) continue;

        var route_copy = handler_contract.ApiRouteInfo{
            .method = try allocator.dupe(u8, route.method),
            .path = try allocator.dupe(u8, route.path),
            .request_schema_refs = .empty,
            .request_schema_dynamic = route.request_schema_dynamic,
            .requires_bearer = route.requires_bearer,
            .requires_jwt = route.requires_jwt,
            .response_status = route.response_status,
            .response_content_type = if (route.response_content_type) |content_type|
                try allocator.dupe(u8, content_type)
            else
                null,
        };
        errdefer route_copy.deinit(allocator);

        for (route.request_schema_refs.items) |schema_ref| {
            try appendUniqueString(allocator, &route_copy.request_schema_refs, schema_ref, false);
        }

        try target.api.routes.append(allocator, route_copy);
    }
}

fn getOrCreateFunctionEntry(
    allocator: std.mem.Allocator,
    functions: *std.ArrayList(HandlerContract.FunctionEntry),
    module_name: []const u8,
) !*HandlerContract.FunctionEntry {
    for (functions.items) |*entry| {
        if (std.mem.eql(u8, entry.module, module_name)) return entry;
    }

    try functions.append(allocator, .{
        .module = try allocator.dupe(u8, module_name),
        .names = .empty,
    });
    return &functions.items[functions.items.len - 1];
}

fn appendUniqueString(
    allocator: std.mem.Allocator,
    list: *std.ArrayList([]const u8),
    value: []const u8,
    case_insensitive: bool,
) !void {
    for (list.items) |item| {
        const matches = if (case_insensitive)
            std.ascii.eqlIgnoreCase(item, value)
        else
            std.mem.eql(u8, item, value);
        if (matches) return;
    }
    try list.append(allocator, try allocator.dupe(u8, value));
}

fn appendSqlQuery(
    allocator: std.mem.Allocator,
    list: *std.ArrayList(handler_contract.SqlQueryInfo),
    query: handler_contract.SqlQueryInfo,
) !void {
    for (list.items) |*existing| {
        if (!std.mem.eql(u8, existing.name, query.name)) continue;
        if (!std.mem.eql(u8, existing.statement, query.statement)) return error.DuplicateSqlQueryName;
        return;
    }

    var copy = handler_contract.SqlQueryInfo{
        .name = try allocator.dupe(u8, query.name),
        .statement = try allocator.dupe(u8, query.statement),
        .operation = query.operation,
        .tables = .empty,
    };
    errdefer copy.deinit(allocator);

    for (query.tables.items) |table| {
        try appendUniqueString(allocator, &copy.tables, table, false);
    }

    try list.append(allocator, copy);
}

fn upsertApiSchema(
    allocator: std.mem.Allocator,
    list: *std.ArrayList(handler_contract.ApiSchemaInfo),
    name: []const u8,
    schema_json: []const u8,
) !void {
    for (list.items) |*schema| {
        if (!std.mem.eql(u8, schema.name, name)) continue;
        allocator.free(schema.schema_json);
        schema.schema_json = try allocator.dupe(u8, schema_json);
        return;
    }

    try list.append(allocator, .{
        .name = try allocator.dupe(u8, name),
        .schema_json = try allocator.dupe(u8, schema_json),
    });
}

fn hasApiRoute(routes: []const handler_contract.ApiRouteInfo, method: []const u8, path: []const u8) bool {
    for (routes) |route| {
        if (std.mem.eql(u8, route.method, method) and std.mem.eql(u8, route.path, path)) return true;
    }
    return false;
}

fn validateSqlContract(
    allocator: std.mem.Allocator,
    contract: *HandlerContract,
    sql_schema_path: ?[]const u8,
) !void {
    if (contract.sql.queries.items.len == 0) return;

    const schema_path = sql_schema_path orelse {
        if (!builtin.is_test) {
            std.debug.print("zigttp:sql queries require --sql-schema <schema.sql|schema.sqlite>\n", .{});
        }
        return error.MissingSqlSchema;
    };

    var db = try openSqlSchemaDatabase(allocator, schema_path);
    defer db.close();

    for (contract.sql.queries.items) |*query| {
        var analysis = sql_analysis.analyzeStatement(allocator, query.statement) catch |err| {
            if (!builtin.is_test) {
                std.debug.print("Unsupported SQL statement for query '{s}': {s}\n", .{ query.name, query.statement });
            }
            return err;
        };
        defer analysis.deinit(allocator);

        var stmt = db.prepare(query.statement) catch {
            if (!builtin.is_test) {
                std.debug.print("SQL validation failed for query '{s}': {s}\n", .{ query.name, db.errmsg() });
            }
            return error.InvalidSqlQuery;
        };
        defer stmt.finalize();

        try ensureNamedParameters(&stmt, query.name);

        query.operation = analysis.operation.toString();
        for (query.tables.items) |table| allocator.free(table);
        query.tables.clearAndFree(allocator);
        for (analysis.tables.items) |table| {
            try appendUniqueString(allocator, &query.tables, table, false);
        }
    }
}

fn openSqlSchemaDatabase(allocator: std.mem.Allocator, schema_path: []const u8) !sqlite.Db {
    if (std.mem.endsWith(u8, schema_path, ".sql")) {
        const schema_source = try readFilePosix(allocator, schema_path, 10 * 1024 * 1024);
        defer allocator.free(schema_source);

        var db = try sqlite.Db.openInMemory();
        errdefer db.close();
        try db.exec(allocator, schema_source);
        return db;
    }

    return sqlite.Db.openReadOnly(allocator, schema_path);
}

fn ensureNamedParameters(stmt: *sqlite.Stmt, query_name: []const u8) !void {
    const count = stmt.paramCount();
    for (1..count + 1) |idx| {
        if (stmt.paramName(idx) == null) {
            if (!builtin.is_test) {
                std.debug.print("SQL query '{s}' uses positional parameters; only named parameters are supported\n", .{query_name});
            }
            return error.PositionalSqlParameter;
        }
    }
}

fn enforcePolicyForContract(
    allocator: std.mem.Allocator,
    label_path: []const u8,
    contract: *const HandlerContract,
    policy: ?HandlerPolicy,
) !void {
    const p = policy orelse return;

    var report = try handler_policy.validateContract(allocator, contract, &p);
    defer report.deinit(allocator);

    if (!report.hasViolations()) return;

    if (!@import("builtin").is_test) {
        std.debug.print("\nCapability policy violations in {s}:\n", .{label_path});
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
        handler_policy.formatViolations(&report, &aw.writer) catch {};
        output = aw.toArrayList();
        if (output.items.len > 0) {
            std.debug.print("{s}", .{output.items});
        }
    }
    return error.PolicyViolation;
}

fn printSandboxReport(contract: *const HandlerContract) void {
    const env_restricted = !contract.env.dynamic;
    const egress_restricted = !contract.egress.dynamic;
    const cache_restricted = !contract.cache.dynamic;
    const sql_restricted = !contract.sql.dynamic;

    if (env_restricted and egress_restricted and cache_restricted and sql_restricted) {
        std.debug.print("Sandbox: complete (all access statically proven)\n", .{});
    } else {
        std.debug.print("Sandbox derived from contract:\n", .{});
    }

    printSandboxSection("env", contract.env.literal.items, env_restricted, "no dynamic access");
    printSandboxSection("egress", contract.egress.hosts.items, egress_restricted, "no dynamic access");
    printSandboxSection("cache", contract.cache.namespaces.items, cache_restricted, "no dynamic access");
    printSqlSandboxSection(contract);
}

fn printPropertiesReport(contract: *const HandlerContract) void {
    const props = contract.properties orelse return;

    std.debug.print("Handler Properties:\n", .{});

    const fields = [_]struct { name: []const u8, value: bool, desc: []const u8 }{
        .{ .name = "pure", .value = props.pure, .desc = "handler is a deterministic function of the request" },
        .{ .name = "read_only", .value = props.read_only, .desc = "no state mutations via virtual modules" },
        .{ .name = "stateless", .value = props.stateless, .desc = "independent of mutable state" },
        .{ .name = "retry_safe", .value = props.retry_safe, .desc = "safe for Lambda auto-retry on timeout" },
        .{ .name = "deterministic", .value = props.deterministic, .desc = "no Date.now() or Math.random()" },
        .{ .name = "idempotent", .value = props.idempotent, .desc = "safe for at-least-once delivery" },
    };

    for (fields) |f| {
        const label = if (f.value) "PROVEN" else "---   ";
        std.debug.print("  {s} {s: <15} {s}\n", .{ label, f.name, f.desc });
    }

    if (props.max_io_depth) |depth| {
        std.debug.print("  PROVEN {s: <15} max {d} I/O calls per request\n", .{ "max_io_depth", depth });
    }
}

fn printSqlSandboxSection(contract: *const HandlerContract) void {
    if (contract.sql.dynamic) {
        std.debug.print("  sql: unrestricted (dynamic access detected)\n", .{});
        return;
    }
    if (contract.sql.queries.items.len == 0) {
        std.debug.print("  sql: restricted to [] (none proven, no dynamic access)\n", .{});
        return;
    }

    std.debug.print("  sql: restricted to [", .{});
    for (contract.sql.queries.items, 0..) |query, idx| {
        if (idx > 0) std.debug.print(", ", .{});
        std.debug.print("{s}", .{query.name});
    }
    std.debug.print("] ({d} proven, no dynamic access)\n", .{contract.sql.queries.items.len});
}

fn printSandboxSection(name: []const u8, items: []const []const u8, restricted: bool, reason: []const u8) void {
    if (!restricted) {
        std.debug.print("  {s}: unrestricted (dynamic access detected)\n", .{name});
        return;
    }
    if (items.len == 0) {
        std.debug.print("  {s}: restricted to [] (none proven, {s})\n", .{ name, reason });
        return;
    }
    std.debug.print("  {s}: restricted to [", .{name});
    for (items, 0..) |item, i| {
        if (i > 0) std.debug.print(", ", .{});
        std.debug.print("{s}", .{item});
    }
    std.debug.print("] ({d} proven, {s})\n", .{ items.len, reason });
}

/// Derive contract.json path from the output .zig path.
/// e.g. "src/generated/embedded_handler.zig" -> "src/generated/contract.json"
/// Derive a sibling path by replacing the filename portion with a suffix.
/// E.g. deriveSiblingPath("src/generated/foo.zig", "contract.json") -> "src/generated/contract.json"
fn deriveSiblingPath(allocator: std.mem.Allocator, output_path: []const u8, suffix: []const u8) ![]u8 {
    var dir_end: usize = 0;
    for (output_path, 0..) |c, i| {
        if (c == '/') dir_end = i + 1;
    }
    const dir = output_path[0..dir_end];
    const result = try allocator.alloc(u8, dir.len + suffix.len);
    @memcpy(result[0..dir.len], dir);
    @memcpy(result[dir.len..], suffix);
    return result;
}

fn writeZigFile(
    path: []const u8,
    compiled: CompiledHandler,
    handler_path: []const u8,
    policy: ?HandlerPolicy,
    allocator: std.mem.Allocator,
) !void {
    // If the transpiler produced output, use it directly
    if (compiled.transpiled_source) |transpiled| {
        var output = std.ArrayList(u8).empty;
        defer output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
        const writer = &aw.writer;

        // Write the transpiled source (already contains header, helpers, handler)
        try writer.writeAll(transpiled);

        // Append bytecode for interpreter fallback
        try writer.writeAll("\npub const bytecode = [_]u8{\n");
        try writeBytecodeArray(writer, compiled.bytecode);
        try writer.writeAll("};\n");

        // Append dependency module declarations (required by zruntime.zig)
        try writer.writeAll("\npub const dep_count: u16 = 0;\n");
        try writer.writeAll("pub const dep_bytecodes = [_][]const u8{};\n");
        const contract_ptr = if (compiled.contract) |*c| c else null;
        try writeCapabilityPolicy(writer, policy, contract_ptr);

        output = aw.toArrayList();
        try writeFilePosix(path, output.items, allocator);
        std.debug.print("Wrote transpiled handler to: {s}\n", .{path});
        return;
    }

    // Fall back to pattern-matching AOT code generation
    var output = std.ArrayList(u8).empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    const writer = &aw.writer;

    const has_aot = if (compiled.aot) |analysis| blk: {
        if (analysis.default_response != null) break :blk true;
        if (analysis.dispatch) |dispatch| break :blk countAotPatterns(dispatch) > 0;
        break :blk false;
    } else false;

    try writer.writeAll("//! Auto-generated embedded handler bytecode\n");
    try writer.writeAll("//! Do not edit - regenerate with: zig build -Dhandler=<path>\n");
    if (has_aot) {
        try writer.writeAll("//! AOT enabled - regenerate with: zig build -Dhandler=<path> -Daot=true\n\n");
    } else {
        try writer.writeAll("\n");
    }

    try writer.writeAll("const std = @import(\"std\");\n");
    try writer.writeAll("const zq = @import(\"zts\");\n\n");

    if (has_aot) {
        try writer.writeAll("pub const has_aot = true;\n");
        try writer.writeAll("pub const aot_metadata = .{\n");
        try writer.writeAll("    .handler_path = ");
        try writeZigStringLiteral(writer, handler_path);
        try writer.writeAll(",\n");
        if (compiled.aot.?.handler_loc) |loc| {
            try writer.print("    .handler_line = {d},\n", .{loc.line});
            try writer.print("    .handler_column = {d},\n", .{loc.column});
        } else {
            try writer.writeAll("    .handler_line = 0,\n");
            try writer.writeAll("    .handler_column = 0,\n");
        }
        const pattern_count = if (compiled.aot.?.dispatch) |dispatch|
            countAotPatterns(dispatch)
        else
            @as(usize, 0);
        try writer.print("    .pattern_count = {d},\n", .{pattern_count});
        try writer.writeAll("    .has_default = ");
        try writer.writeAll(if (compiled.aot.?.default_response != null) "true,\n" else "false,\n");
        try writer.writeAll("};\n\n");

        try writer.writeAll("pub fn aotHandler(ctx: *zq.Context, args: []const zq.JSValue) anyerror!zq.JSValue {\n");
        try writer.writeAll("    if (args.len < 1) return error.AotBail;\n");
        try writer.writeAll("    const req_val = args[0];\n");
        try writer.writeAll("    if (!req_val.isObject()) return error.AotBail;\n");
        try writer.writeAll("    const req_obj = req_val.toPtr(zq.JSObject);\n");
        try writer.writeAll("    const pool = ctx.hidden_class_pool orelse return error.AotBail;\n");
        try writer.writeAll("    var url_val: zq.JSValue = zq.JSValue.undefined_val;\n");
        try writer.writeAll("    var path_val: zq.JSValue = zq.JSValue.undefined_val;\n");
        try writer.writeAll("    if (ctx.http_shapes) |shapes| {\n");
        try writer.writeAll("        if (req_obj.hidden_class_idx == shapes.request.class_idx) {\n");
        try writer.writeAll("            url_val = req_obj.getSlot(shapes.request.url_slot);\n");
        try writer.writeAll("            path_val = req_obj.getSlot(shapes.request.path_slot);\n");
        try writer.writeAll("        } else if (req_obj.getOwnProperty(pool, zq.Atom.url)) |val| {\n");
        try writer.writeAll("            url_val = val;\n");
        try writer.writeAll("            if (req_obj.getOwnProperty(pool, zq.Atom.path)) |path_prop| {\n");
        try writer.writeAll("                path_val = path_prop;\n");
        try writer.writeAll("            }\n");
        try writer.writeAll("        } else {\n");
        try writer.writeAll("            return error.AotBail;\n");
        try writer.writeAll("        }\n");
        try writer.writeAll("    } else if (req_obj.getOwnProperty(pool, zq.Atom.url)) |val| {\n");
        try writer.writeAll("        url_val = val;\n");
        try writer.writeAll("        if (req_obj.getOwnProperty(pool, zq.Atom.path)) |path_prop| {\n");
        try writer.writeAll("            path_val = path_prop;\n");
        try writer.writeAll("        }\n");
        try writer.writeAll("    } else {\n");
        try writer.writeAll("        return error.AotBail;\n");
        try writer.writeAll("    }\n");
        try writer.writeAll("    if (!url_val.isString()) return error.AotBail;\n");
        try writer.writeAll("    const url = url_val.toPtr(zq.JSString).data();\n");
        try writer.writeAll("    const path = if (path_val.isString()) path_val.toPtr(zq.JSString).data() else url;\n");

        if (compiled.aot.?.dispatch) |dispatch| {
            for (dispatch.patterns) |pattern| {
                switch (pattern.pattern_type) {
                    .exact => {
                        const route_target = if (pattern.url_atom == .path) "path" else "url";
                        try writer.writeAll("    if (std.mem.eql(u8, ");
                        try writer.writeAll(route_target);
                        try writer.writeAll(", ");
                        try writeZigStringLiteral(writer, pattern.url_bytes);
                        try writer.writeAll(")) {\n");
                        switch (pattern.body_source) {
                            .static => {
                                try writer.writeAll("        return zq.http.createResponse(ctx, ");
                                try writeZigStringLiteral(writer, pattern.static_body);
                                try writer.writeAll(", ");
                                try writer.print("{d}, ", .{pattern.status});
                                try writeZigStringLiteral(writer, contentTypeFor(pattern.content_type_idx));
                                try writer.writeAll(");\n");
                            },
                            .request_json_parse => {
                                try writer.writeAll("        var body_val: zq.JSValue = zq.JSValue.undefined_val;\n");
                                try writer.writeAll("        if (ctx.http_shapes) |shapes| {\n");
                                try writer.writeAll("            if (req_obj.hidden_class_idx == shapes.request.class_idx) {\n");
                                try writer.writeAll("                body_val = req_obj.getSlot(shapes.request.body_slot);\n");
                                try writer.writeAll("            } else if (req_obj.getOwnProperty(pool, zq.Atom.body)) |val| {\n");
                                try writer.writeAll("                body_val = val;\n");
                                try writer.writeAll("            } else {\n");
                                try writer.writeAll("                return error.AotBail;\n");
                                try writer.writeAll("            }\n");
                                try writer.writeAll("        } else if (req_obj.getOwnProperty(pool, zq.Atom.body)) |val| {\n");
                                try writer.writeAll("            body_val = val;\n");
                                try writer.writeAll("        } else {\n");
                                try writer.writeAll("            return error.AotBail;\n");
                                try writer.writeAll("        }\n");
                                try writer.writeAll("        if (!body_val.isString()) return error.AotBail;\n");
                                try writer.writeAll("        const parse_args = [_]zq.JSValue{body_val};\n");
                                try writer.writeAll("        const parsed = zq.builtins.jsonParse(ctx, zq.JSValue.undefined_val, &parse_args);\n");
                                try writer.writeAll("        if (parsed.isUndefined()) return error.AotBail;\n");
                                try writer.writeAll("        const json_body = zq.http.valueToJsonString(ctx, parsed) catch return error.AotBail;\n");
                                try writer.writeAll("        return zq.http.createResponseFromString(ctx, json_body, ");
                                try writer.print("{d}", .{pattern.status});
                                try writer.writeAll(", ");
                                try writeZigStringLiteral(writer, contentTypeFor(pattern.content_type_idx));
                                try writer.writeAll(");\n");
                            },
                        }
                        try writer.writeAll("    }\n");
                    },
                    .prefix => {
                        if (pattern.response_template_prefix == null) continue;
                        const prefix = pattern.url_bytes;
                        const tpl_prefix = pattern.response_template_prefix.?;
                        const tpl_suffix = pattern.response_template_suffix orelse "";
                        const route_target = if (pattern.url_atom == .path) "path" else "url";
                        try writer.writeAll("    if (std.mem.startsWith(u8, ");
                        try writer.writeAll(route_target);
                        try writer.writeAll(", ");
                        try writeZigStringLiteral(writer, prefix);
                        try writer.writeAll(")) {\n");
                        try writer.writeAll("        const param = ");
                        try writer.writeAll(route_target);
                        try writer.writeAll("[");
                        try writer.print("{d}", .{prefix.len});
                        try writer.writeAll("..];\n");
                        try writer.writeAll("        const body_len = ");
                        try writer.print("{d}", .{tpl_prefix.len});
                        try writer.writeAll(" + param.len + ");
                        try writer.print("{d}", .{tpl_suffix.len});
                        try writer.writeAll(";\n");
                        try writer.writeAll("        var body = try ctx.allocator.alloc(u8, body_len);\n");
                        try writer.writeAll("        defer ctx.allocator.free(body);\n");
                        try writer.writeAll("        @memcpy(body[0..");
                        try writer.print("{d}", .{tpl_prefix.len});
                        try writer.writeAll("], ");
                        try writeZigStringLiteral(writer, tpl_prefix);
                        try writer.writeAll(");\n");
                        try writer.writeAll("        @memcpy(body[");
                        try writer.print("{d}", .{tpl_prefix.len});
                        try writer.writeAll("..][0..param.len], param);\n");
                        try writer.writeAll("        @memcpy(body[");
                        try writer.print("{d}", .{tpl_prefix.len});
                        try writer.writeAll(" + param.len ..][0..");
                        try writer.print("{d}", .{tpl_suffix.len});
                        try writer.writeAll("], ");
                        try writeZigStringLiteral(writer, tpl_suffix);
                        try writer.writeAll(");\n");
                        try writer.writeAll("        return zq.http.createResponse(ctx, body, ");
                        try writer.print("{d}, ", .{pattern.status});
                        try writeZigStringLiteral(writer, contentTypeFor(pattern.content_type_idx));
                        try writer.writeAll(");\n");
                        try writer.writeAll("    }\n");
                    },
                    else => {},
                }
            }
        }

        if (compiled.aot.?.default_response) |resp| {
            try writer.writeAll("    return zq.http.createResponse(ctx, ");
            try writeZigStringLiteral(writer, resp.body);
            try writer.writeAll(", ");
            try writer.print("{d}, ", .{resp.status});
            try writeZigStringLiteral(writer, contentTypeFor(resp.content_type_idx));
            try writer.writeAll(");\n");
        } else {
            try writer.writeAll("    return error.AotBail;\n");
        }
        try writer.writeAll("}\n\n");
    } else {
        try writer.writeAll("pub const has_aot = false;\n");
        try writer.writeAll("pub fn aotHandler(_: *zq.Context, _: []const zq.JSValue) anyerror!zq.JSValue {\n");
        try writer.writeAll("    return error.AotBail;\n");
        try writer.writeAll("}\n\n");
    }

    try writer.writeAll("pub const bytecode = [_]u8{\n");
    try writeBytecodeArray(writer, compiled.bytecode);
    try writer.writeAll("};\n");

    // Write dependency module bytecodes if present
    if (compiled.dep_bytecodes) |deps| {
        try writer.print("\npub const dep_count: u16 = {d};\n\n", .{deps.len});
        for (deps, 0..) |dep, i| {
            try writer.print("const dep_{d} = [_]u8{{\n", .{i});
            try writeBytecodeArray(writer, dep);
            try writer.writeAll("};\n\n");
        }
        try writer.print("pub const dep_bytecodes = [_][]const u8{{\n", .{});
        for (0..deps.len) |i| {
            try writer.print("    &dep_{d},\n", .{i});
        }
        try writer.writeAll("};\n");
    } else {
        try writer.writeAll("\npub const dep_count: u16 = 0;\n");
        try writer.writeAll("pub const dep_bytecodes = [_][]const u8{};\n");
    }

    const contract_ptr = if (compiled.contract) |*c| c else null;
    try writeCapabilityPolicy(writer, policy, contract_ptr);

    output = aw.toArrayList();
    try writeFilePosix(path, output.items, allocator);
}

fn writeCapabilityPolicy(writer: anytype, policy: ?HandlerPolicy, contract: ?*const HandlerContract) !void {
    try writer.writeAll("\npub const capability_policy = @import(\"zts\").handler_policy.RuntimePolicy{\n");
    if (policy) |p| {
        // Explicit policy takes precedence
        try writePolicySectionFromAllowList(writer, "env", p.env);
        try writePolicySectionFromAllowList(writer, "egress", p.egress);
        try writePolicySectionFromAllowList(writer, "cache", p.cache);
        try writePolicySectionFromAllowList(writer, "sql", p.sql);
    } else if (contract) |c| {
        // Auto-derive from contract proven facts
        try writeContractDerivedSection(writer, "env", c.env.literal.items, c.env.dynamic);
        try writeContractDerivedSection(writer, "egress", c.egress.hosts.items, c.egress.dynamic);
        try writeContractDerivedSection(writer, "cache", c.cache.namespaces.items, c.cache.dynamic);
        try writeSqlContractDerivedSection(writer, c);
    } else {
        // No policy, no contract: permissive
        try writePolicySectionFromAllowList(writer, "env", null);
        try writePolicySectionFromAllowList(writer, "egress", null);
        try writePolicySectionFromAllowList(writer, "cache", null);
        try writePolicySectionFromAllowList(writer, "sql", null);
    }
    try writer.writeAll("};\n");
}

fn writePolicySectionFromAllowList(writer: anytype, field_name: []const u8, section: ?handler_policy.AllowList) !void {
    try writer.print("    .{s} = ", .{field_name});
    if (section) |allow| {
        try writer.writeAll(".{\n");
        try writer.writeAll("        .enabled = true,\n");
        try writer.writeAll("        .values = &[_][]const u8{\n");
        for (allow.values.items) |item| {
            try writer.writeAll("            ");
            try writeZigStringLiteral(writer, item);
            try writer.writeAll(",\n");
        }
        try writer.writeAll("        },\n");
        try writer.writeAll("    },\n");
        return;
    }
    try writer.writeAll(".{},\n");
}

/// Write a policy section derived from contract proven facts.
/// When dynamic is false, restrict to exactly the proven literals.
/// When dynamic is true, leave permissive.
fn writeContractDerivedSection(writer: anytype, field_name: []const u8, literals: []const []const u8, dynamic: bool) !void {
    try writer.print("    .{s} = ", .{field_name});
    if (!dynamic) {
        // Static: restrict to proven literals
        try writer.writeAll(".{\n");
        try writer.writeAll("        .enabled = true,\n");
        try writer.writeAll("        .values = &[_][]const u8{\n");
        for (literals) |item| {
            try writer.writeAll("            ");
            try writeZigStringLiteral(writer, item);
            try writer.writeAll(",\n");
        }
        try writer.writeAll("        },\n");
        try writer.writeAll("    },\n");
    } else {
        // Dynamic: can't restrict
        try writer.writeAll(".{},\n");
    }
}

fn writeSqlContractDerivedSection(writer: anytype, contract: *const HandlerContract) !void {
    try writer.writeAll("    .sql = ");
    if (!contract.sql.dynamic) {
        try writer.writeAll(".{\n");
        try writer.writeAll("        .enabled = true,\n");
        try writer.writeAll("        .values = &[_][]const u8{\n");
        for (contract.sql.queries.items) |query| {
            try writer.writeAll("            ");
            try writeZigStringLiteral(writer, query.name);
            try writer.writeAll(",\n");
        }
        try writer.writeAll("        },\n");
        try writer.writeAll("    },\n");
    } else {
        try writer.writeAll(".{},\n");
    }
}

fn writeBytecodeArray(writer: anytype, bytecode_data: []const u8) !void {
    var i: usize = 0;
    while (i < bytecode_data.len) {
        try writer.writeAll("    ");
        const row_end = @min(i + 16, bytecode_data.len);
        while (i < row_end) {
            const byte = bytecode_data[i];
            var buf: [6]u8 = undefined;
            buf[0] = '0';
            buf[1] = 'x';
            buf[2] = hexChar(@truncate(byte >> 4));
            buf[3] = hexChar(@truncate(byte & 0x0f));
            buf[4] = ',';
            buf[5] = ' ';
            try writer.writeAll(buf[0..]);
            i += 1;
        }
        try writer.writeAll("\n");
    }
}

fn writeZigStringLiteral(writer: anytype, value: []const u8) !void {
    try writer.writeAll("\"");
    for (value) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00...0x08, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                var buf: [4]u8 = undefined;
                buf[0] = '\\';
                buf[1] = 'x';
                buf[2] = hexChar(@truncate(c >> 4));
                buf[3] = hexChar(@truncate(c & 0x0f));
                try writer.writeAll(&buf);
            },
            else => try writer.writeByte(c),
        }
    }
    try writer.writeAll("\"");
}

fn contentTypeFor(idx: u8) []const u8 {
    return switch (idx) {
        0 => "application/json",
        1 => "text/plain; charset=utf-8",
        else => "text/html; charset=utf-8",
    };
}

fn hexChar(n: u4) u8 {
    const hex_chars = "0123456789abcdef";
    return hex_chars[n];
}

test "compileHandler aggregates contract across file imports" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const entry_source =
        \\import { readSecret } from "./dep.js";
        \\export function handler(req) {
        \\  return Response.text(readSecret());
        \\}
    ;
    const dep_source =
        \\import { env } from "zigttp:env";
        \\export function readSecret() {
        \\  return env("JWT_SECRET") ?? "";
        \\}
    ;

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "entry.js", .data = entry_source });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "dep.js", .data = dep_source });

    const allocator = std.testing.allocator;
    const entry_path = try tmp.dir.realPathFileAlloc(std.testing.io, "entry.js", allocator);
    defer allocator.free(entry_path);

    var policy = try handler_policy.parsePolicyJson(allocator, "{\"env\":{\"allow\":[\"JWT_SECRET\"]}}");
    defer policy.deinit(allocator);

    var compiled = try compileHandler(
        allocator,
        entry_source,
        entry_path,
        false,
        false,
        true,
        policy,
        null,
        false,
    );
    defer compiled.deinit(allocator);

    try std.testing.expect(compiled.dep_bytecodes != null);
    const contract = compiled.contract orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(usize, 1), contract.env.literal.items.len);
    try std.testing.expectEqualStrings("JWT_SECRET", contract.env.literal.items[0]);
}

test "compileHandler rejects disallowed policy from imported module" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const entry_source =
        \\import { readSecret } from "./dep.js";
        \\export function handler(req) {
        \\  return Response.text(readSecret());
        \\}
    ;
    const dep_source =
        \\import { env } from "zigttp:env";
        \\export function readSecret() {
        \\  return env("JWT_SECRET") ?? "";
        \\}
    ;

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "entry.js", .data = entry_source });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "dep.js", .data = dep_source });

    const allocator = std.testing.allocator;
    const entry_path = try tmp.dir.realPathFileAlloc(std.testing.io, "entry.js", allocator);
    defer allocator.free(entry_path);

    var policy = try handler_policy.parsePolicyJson(allocator, "{\"env\":{\"allow\":[\"PUBLIC_KEY\"]}}");
    defer policy.deinit(allocator);

    try std.testing.expectError(
        error.PolicyViolation,
        compileHandler(
            allocator,
            entry_source,
            entry_path,
            false,
            false,
            false,
            policy,
            null,
            false,
        ),
    );
}

fn buildTestContractForSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    sql_schema_path: ?[]const u8,
) !HandlerContract {
    var strings = zts.StringTable.init(allocator);
    defer strings.deinit();

    var atoms = zts.context.AtomTable.init(allocator);
    defer atoms.deinit();

    var js_parser = zts.parser.JsParser.init(allocator, source);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);

    const root = try js_parser.parse();
    return buildContractWithPolicy(
        allocator,
        &js_parser,
        &atoms,
        filename,
        root,
        null,
        null,
        null,
        null,
        sql_schema_path,
    );
}

test "buildContractWithPolicy validates zigttp:sql queries against schema" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const source =
        \\import { sql } from "zigttp:sql";
        \\
        \\sql("createUser", "INSERT INTO users (name) VALUES (:name)");
        \\sql("getUser", "SELECT id, name FROM users WHERE id = :id");
        \\
        \\export function handler(req) {
        \\  return Response.json({ ok: true });
        \\}
    ;
    const schema =
        \\CREATE TABLE users (
        \\  id INTEGER PRIMARY KEY,
        \\  name TEXT NOT NULL
        \\);
    ;

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "handler.js", .data = source });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "schema.sql", .data = schema });

    const allocator = std.testing.allocator;
    const entry_path = try tmp.dir.realPathFileAlloc(std.testing.io, "handler.js", allocator);
    defer allocator.free(entry_path);
    const schema_path = try tmp.dir.realPathFileAlloc(std.testing.io, "schema.sql", allocator);
    defer allocator.free(schema_path);

    var contract = try buildTestContractForSource(allocator, source, entry_path, schema_path);
    defer contract.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 2), contract.sql.queries.items.len);
    try std.testing.expectEqualStrings("createUser", contract.sql.queries.items[0].name);
    try std.testing.expectEqualStrings("insert", contract.sql.queries.items[0].operation);
    try std.testing.expectEqual(@as(usize, 1), contract.sql.queries.items[0].tables.items.len);
    try std.testing.expectEqualStrings("users", contract.sql.queries.items[0].tables.items[0]);
    try std.testing.expectEqualStrings("getUser", contract.sql.queries.items[1].name);
    try std.testing.expectEqualStrings("select", contract.sql.queries.items[1].operation);
}

test "buildContractWithPolicy requires sql schema when zigttp:sql is used" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const source =
        \\import { sql } from "zigttp:sql";
        \\
        \\sql("getUser", "SELECT id, name FROM users WHERE id = :id");
        \\
        \\export function handler(req) {
        \\  return Response.json({ ok: true });
        \\}
    ;

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "handler.js", .data = source });

    const allocator = std.testing.allocator;
    const entry_path = try tmp.dir.realPathFileAlloc(std.testing.io, "handler.js", allocator);
    defer allocator.free(entry_path);

    try std.testing.expectError(
        error.MissingSqlSchema,
        buildTestContractForSource(allocator, source, entry_path, null),
    );
}
