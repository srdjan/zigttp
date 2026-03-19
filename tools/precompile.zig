//! Build-time TypeScript/JavaScript precompiler
//!
//! Compiles handler files to bytecode at build time for embedding in the binary.
//! This eliminates runtime parsing overhead and removes the need for source files
//! in deployment.
//!
//! Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--policy policy.json] <handler.ts> <output.zig>

const std = @import("std");
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
        if (self.bytecode.len > 0) {
            allocator.free(self.bytecode);
        }
        // Note: transpiled_source is owned by the transpiler, not freed here
    }
};

/// Read a file synchronously using posix operations
fn readFilePosix(allocator: std.mem.Allocator, path: []const u8, max_size: usize) ![]u8 {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(std.posix.AT.FDCWD, path_z, .{ .ACCMODE = .RDONLY }, 0);
    defer std.Io.Threaded.closeFd(fd);

    var buffer: std.ArrayList(u8) = .empty;
    errdefer buffer.deinit(allocator);

    var chunk: [4096]u8 = undefined;
    while (true) {
        const bytes_read = try std.posix.read(fd, &chunk);
        if (bytes_read == 0) break;
        if (buffer.items.len + bytes_read > max_size) {
            return error.FileTooBig;
        }
        try buffer.appendSlice(allocator, chunk[0..bytes_read]);
    }

    return buffer.toOwnedSlice(allocator);
}

/// Write a file synchronously using posix operations
fn writeFilePosix(path: []const u8, data: []const u8, allocator: std.mem.Allocator) !void {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const fd = try std.posix.openatZ(
        std.posix.AT.FDCWD,
        path_z,
        .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true },
        0o644,
    );
    defer std.Io.Threaded.closeFd(fd);
    if (std.c.ftruncate(fd, 0) != 0) return error.WriteFailure;

    var total_written: usize = 0;
    while (total_written < data.len) {
        const result = std.c.write(fd, data[total_written..].ptr, data.len - total_written);
        if (result < 0) return error.WriteFailure;
        if (result == 0) return error.WriteFailure;
        total_written += @intCast(result);
    }
}

pub fn main(init: std.process.Init.Minimal) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    var args = std.process.Args.Iterator.init(init.args);
    defer args.deinit();

    // Skip program name
    _ = args.skip();

    var emit_aot = false;
    var emit_verify = false;
    var emit_contract = false;
    var emit_openapi = false;
    var policy_path: ?[]const u8 = null;
    var deploy_target_str: ?[]const u8 = null;
    var replay_trace_path: ?[]const u8 = null;
    var handler_path: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--aot")) {
            emit_aot = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--verify")) {
            emit_verify = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--contract")) {
            emit_contract = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--openapi")) {
            emit_openapi = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--policy")) {
            policy_path = args.next() orelse {
                std.debug.print("Missing path after --policy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--deploy")) {
            deploy_target_str = args.next() orelse {
                std.debug.print("Missing target after --deploy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--replay")) {
            replay_trace_path = args.next() orelse {
                std.debug.print("Missing path after --replay\n", .{});
                return error.MissingArgument;
            };
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

        std.debug.print("Unexpected argument: {s}\n", .{arg});
        return error.InvalidArgument;
    }

    const handler_path_final = handler_path orelse {
        std.debug.print("Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--policy policy.json] <handler.ts> <output.zig>\n", .{});
        std.debug.print("\nCompiles a TypeScript/JavaScript handler to bytecode.\n", .{});
        return error.MissingArgument;
    };

    const output_path_final = output_path orelse {
        std.debug.print("Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--policy policy.json] <handler.ts> <output.zig>\n", .{});
        std.debug.print("\nMissing output path.\n", .{});
        return error.MissingArgument;
    };

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
    var compiled = compileHandler(
        allocator,
        source,
        handler_path_final,
        emit_aot,
        emit_verify,
        emit_contract,
        policy,
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
/// Creates a lightweight zts context for each trace, loads the handler source,
/// installs replay stubs, executes, and compares responses.
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

/// Replay a single trace at build time using a standalone zts context.
fn replayOneBuildTime(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    group: *const zts.trace.RequestTraceGroup,
) !bool {
    // Create a standalone zts context
    const ctx = try zts.createContext(allocator, .{ .nursery_size = 64 * 1024 });
    defer zts.destroyContext(ctx);
    try zts.builtins.initBuiltins(ctx);

    // Register replay stubs for all virtual modules
    inline for (std.meta.fields(zts.modules.VirtualModule)) |field| {
        const module: zts.modules.VirtualModule = @enumFromInt(field.value);
        try zts.modules.registerVirtualModuleReplay(module, ctx, allocator);
    }

    // Set up replay state
    var replay_state = zts.trace.ReplayState{
        .io_calls = group.io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    ctx.setModuleState(
        zts.trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &zts.trace.ReplayState.deinitOpaque,
    );
    defer ctx.module_state[zts.trace.REPLAY_STATE_SLOT] = null;

    // Parse and compile handler
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

    // Materialize object literal shapes before execution
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

    // Find handler function
    const handler_val = ctx.getGlobal(zts.Atom.handler) orelse return error.NoHandler;
    if (!handler_val.isObject()) return error.NoHandler;
    const handler_obj = zts.JSObject.fromValue(handler_val);
    if (handler_obj.class_id != .function or !handler_obj.flags.is_callable) return error.NoHandler;

    // Build request object using Context methods (same as runtime dynamic path)
    const hc_pool_req = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const req_obj = try ctx.createObject(null);

    const method_val = try ctx.createString(group.request.method);
    try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.method, method_val);

    const url_val = try ctx.createString(group.request.url);
    try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.url, url_val);

    if (group.request.body) |body| {
        const body_val = try ctx.createString(body);
        try req_obj.setProperty(allocator, hc_pool_req, zts.Atom.body, body_val);
    }


    // Execute handler
    const args = [_]zts.JSValue{req_obj.toValue()};
    const bc_data = handler_obj.getBytecodeFunctionData() orelse return error.NotCallable;
    const result = interp.callBytecodeFunction(
        handler_obj.toValue(),
        bc_data.bytecode,
        zts.JSValue.undefined_val,
        &args,
    ) catch return error.HandlerError;

    // Extract response status and body
    if (!result.isObject()) return error.InvalidResponse;
    const result_obj = zts.JSObject.fromValue(result);
    const hc_pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const status_val = result_obj.getProperty(hc_pool, zts.Atom.status) orelse zts.JSValue.fromInt(200);
    const actual_status: u16 = if (status_val.isInt())
        @intCast(@max(0, @min(999, status_val.getInt())))
    else
        200;

    const body_val = result_obj.getProperty(hc_pool, zts.Atom.body) orelse zts.JSValue.undefined_val;
    const actual_body = extractString(body_val) orelse "";

    // Compare against expected
    const expected = group.response orelse return true; // no expected response = pass
    const status_ok = actual_status == expected.status;

    // The trace stores body as a JSON string value with escape sequences.
    // Unescape before comparing against the raw response body.
    const expected_body = zts.trace.unescapeJson(allocator, expected.body) catch expected.body;
    defer if (expected_body.ptr != expected.body.ptr) allocator.free(expected_body);
    const body_ok = std.mem.eql(u8, actual_body, expected_body);

    return status_ok and body_ok and replay_state.divergences == 0;
}

fn extractString(val: zts.JSValue) ?[]const u8 {
    if (val.isString()) return val.toPtr(zts.JSString).data();
    if (val.isStringSlice()) return val.toPtr(zts.string.SliceString).data();
    return null;
}

fn compileHandler(
    allocator: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    emit_aot: bool,
    emit_verify: bool,
    emit_contract: bool,
    policy: ?HandlerPolicy,
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
        std.debug.print("File imports detected, building module graph...\n", .{});
        return compileMultiModule(
            allocator,
            source_to_parse,
            filename,
            &strings,
            &atoms,
            needs_contract,
            emit_contract,
            policy,
        );
    }

    // Optimize IR (cold-start-friendly single pass)
    _ = zts.parser.optimizeIR(
        allocator,
        &js_parser.nodes,
        &js_parser.constants,
        root,
    ) catch {};

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
                    policy,
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
            policy,
        );
    }

    return .{
        .bytecode = bytecode_data,
        .aot = aot,
        .transpiled_source = transpiled_source,
        .contract = contract,
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
) !CompiledHandler {
    // Build module graph
    var graph = zts.modules.ModuleGraph.init(allocator);
    defer graph.deinit();

    graph.build(filename, entry_source, readFilePosixForGraph) catch |err| {
        std.debug.print("Module graph error: {}\n", .{err});
        return err;
    };

    std.debug.print("Module graph: {d} modules ({d} dependencies)\n", .{
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

    std.debug.print("Multi-module bundle: {d} dependency modules + entry\n", .{dep_bytecodes.len});

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
        );
    }

    return .{
        .bytecode = entry_bytecode,
        .dep_bytecodes = dep_bytecodes,
        .contract = contract,
    };
}

/// Read file for module graph - matches ReadFileFn signature
fn readFilePosixForGraph(allocator: std.mem.Allocator, path: []const u8) zts.modules.module_graph.ReadFileError![]const u8 {
    const data = readFilePosix(allocator, path, 10 * 1024 * 1024) catch {
        return error.FileNotFound;
    };
    return data;
}

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

    var builder = ContractBuilder.init(allocator, ir_view, atoms);
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
    policy: ?HandlerPolicy,
) !HandlerContract {
    var contract = try buildContract(
        allocator,
        js_parser,
        atoms,
        filename,
        root,
        aot,
        verify_info,
    );
    errdefer contract.deinit(allocator);

    try enforcePolicyForContract(allocator, filename, &contract, policy);
    if (policy != null) {
        std.debug.print("Capability policy check passed\n", .{});
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
        );
        defer module_contract.deinit(allocator);

        try enforcePolicyForContract(allocator, module.path, &module_contract, policy);
        if (policy != null) policy_checked = true;

        try mergeModuleContract(allocator, &merged, &module_contract, is_entry);
    }

    if (policy_checked) {
        std.debug.print("Capability policy check passed\n", .{});
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

    std.debug.print("\nCapability policy violations in {s}:\n", .{label_path});
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);
    try handler_policy.formatViolations(&report, &aw.writer);
    output = aw.toArrayList();
    if (output.items.len > 0) {
        std.debug.print("{s}", .{output.items});
    }
    return error.PolicyViolation;
}

fn printSandboxReport(contract: *const HandlerContract) void {
    const env_restricted = !contract.env.dynamic;
    const egress_restricted = !contract.egress.dynamic;
    const cache_restricted = !contract.cache.dynamic;

    if (env_restricted and egress_restricted and cache_restricted) {
        std.debug.print("Sandbox: complete (all access statically proven)\n", .{});
    } else {
        std.debug.print("Sandbox derived from contract:\n", .{});
    }

    printSandboxSection("env", contract.env.literal.items, env_restricted, "no dynamic access");
    printSandboxSection("egress", contract.egress.hosts.items, egress_restricted, "no dynamic access");
    printSandboxSection("cache", contract.cache.namespaces.items, cache_restricted, "no dynamic access");
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
    } else if (contract) |c| {
        // Auto-derive from contract proven facts
        try writeContractDerivedSection(writer, "env", c.env.literal.items, c.env.dynamic);
        try writeContractDerivedSection(writer, "egress", c.egress.hosts.items, c.egress.dynamic);
        try writeContractDerivedSection(writer, "cache", c.cache.namespaces.items, c.cache.dynamic);
    } else {
        // No policy, no contract: permissive
        try writePolicySectionFromAllowList(writer, "env", null);
        try writePolicySectionFromAllowList(writer, "egress", null);
        try writePolicySectionFromAllowList(writer, "cache", null);
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
        ),
    );
}
