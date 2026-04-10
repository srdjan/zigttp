//! Build-time TypeScript/JavaScript precompiler
//!
//! Compiles handler files to bytecode at build time for embedding in the binary.
//! This eliminates runtime parsing overhead and removes the need for source files
//! in deployment.
//!
//! Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--sdk ts] [--sql-schema path] [--prove spec] [--policy policy.json] <handler.ts> <output.zig>

const std = @import("std");
const builtin = @import("builtin");
const zigts = @import("zigts");
const ir = zigts.parser;
const IrTranspiler = @import("transpiler.zig").IrTranspiler;
const handler_contract = zigts.handler_contract;
const ContractBuilder = handler_contract.ContractBuilder;
const writeContractJson = handler_contract.writeContractJson;
const HandlerContract = handler_contract.HandlerContract;
const VerificationInfo = handler_contract.VerificationInfo;
const ServiceTypeContext = zigts.service_types.ServiceTypeContext;
const ServiceRouteInfo = zigts.service_types.RouteInfo;
const ServiceResponseVariant = zigts.service_types.ResponseVariant;
const system_linker = zigts.system_linker;
const handler_policy = zigts.handler_policy;
const HandlerPolicy = handler_policy.HandlerPolicy;
const deploy_manifest = @import("deploy_manifest.zig");
const manifest_alignment = @import("manifest_alignment.zig");
const openapi_manifest = @import("openapi_manifest.zig");
const sdk_codegen = @import("sdk_codegen.zig");
const property_expectations = @import("property_expectations.zig");
const prove_upgrade = @import("prove_upgrade.zig");
const build_report = @import("report.zig");
pub const json_diag = @import("json_diagnostics.zig");
const sqlite = zigts.sqlite;
const sql_analysis = zigts.sql_analysis;

const AotAnalysis = struct {
    dispatch: ?*zigts.PatternDispatchTable = null,
    default_response: ?zigts.HandlerAnalyzer.StaticResponseInfo = null,
    handler_loc: ?zigts.parser.SourceLocation = null,

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

pub const CompiledHandler = struct {
    bytecode: []const u8 = &.{},
    /// Set when handler verification fails. violations_jsonl/summary are populated
    /// with counterexample tests. No bytecode is generated.
    verify_failed: bool = false,
    aot: ?AotAnalysis = null,
    transpiled_source: ?[]const u8 = null,
    /// Additional dependency module bytecodes (for file imports).
    /// Stored in execution order, entry module is NOT included.
    dep_bytecodes: ?[]const []const u8 = null,
    /// Contract manifest (when --contract is passed)
    contract: ?HandlerContract = null,
    /// Generated test JSONL (when --generate-tests is passed)
    generated_tests: ?[]const u8 = null,
    /// Counterexample tests for fault coverage violations (when violations exist)
    violations_jsonl: ?[]const u8 = null,
    /// Pre-formatted violations summary for build output (when violations exist)
    violations_summary: ?[]const u8 = null,

    pub fn deinit(self: *CompiledHandler, allocator: std.mem.Allocator) void {
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
        if (self.violations_jsonl) |vj| {
            allocator.free(vj);
        }
        if (self.violations_summary) |vs| {
            allocator.free(vs);
        }
        if (self.bytecode.len > 0) {
            allocator.free(self.bytecode);
        }
        // Note: transpiled_source is owned by the transpiler, not freed here
    }
};

const readFilePosix = zigts.file_io.readFile;

const fileExists = zigts.file_io.fileExists;

pub fn resolveSystemHandlerPaths(
    allocator: std.mem.Allocator,
    system_path: []const u8,
    config: *system_linker.SystemConfig,
) !void {
    const base_dir = std.fs.path.dirname(system_path) orelse ".";
    for (config.handlers) |*entry| {
        if (std.fs.path.isAbsolute(entry.path)) continue;
        if (!fileExists(allocator, entry.path)) {
            const resolved = try std.fs.path.resolve(allocator, &.{ base_dir, entry.path });
            allocator.free(entry.path);
            entry.path = resolved;
        }
    }
}

fn findSchemaJsonByName(contract: *const HandlerContract, schema_ref: []const u8) ?[]const u8 {
    for (contract.api.schemas.items) |schema| {
        if (std.mem.eql(u8, schema.name, schema_ref)) return schema.schema_json;
    }
    return null;
}

const collectRoutePathParamNames = system_linker.collectRoutePathParamNames;

fn buildServiceTypeContextFromContracts(
    allocator: std.mem.Allocator,
    config: *const system_linker.SystemConfig,
    contracts: []const HandlerContract,
) !ServiceTypeContext {
    var route_count: usize = 0;
    for (contracts) |contract| route_count += contract.api.routes.items.len;

    var routes = try allocator.alloc(ServiceRouteInfo, route_count);
    var out_index: usize = 0;
    errdefer {
        for (routes[0..out_index]) |*route| route.deinit(allocator);
        allocator.free(routes);
    }
    for (contracts, config.handlers) |contract, entry| {
        for (contract.api.routes.items) |api_route| {
            var required_path_params = try collectRoutePathParamNames(allocator, &api_route);
            errdefer {
                for (required_path_params.items) |name| allocator.free(name);
                required_path_params.deinit(allocator);
            }

            var required_query_params: std.ArrayList([]const u8) = .empty;
            errdefer {
                for (required_query_params.items) |name| allocator.free(name);
                required_query_params.deinit(allocator);
            }
            for (api_route.query_params.items) |param| {
                if (!param.required) continue;
                try required_query_params.append(allocator, try allocator.dupe(u8, param.name));
            }

            var required_header_params: std.ArrayList([]const u8) = .empty;
            errdefer {
                for (required_header_params.items) |name| allocator.free(name);
                required_header_params.deinit(allocator);
            }
            for (api_route.header_params.items) |param| {
                if (!param.required) continue;
                try required_header_params.append(allocator, try allocator.dupe(u8, param.name));
            }

            var responses = try allocator.alloc(ServiceResponseVariant, api_route.responses.items.len);
            errdefer {
                for (responses) |*response| response.deinit(allocator);
                allocator.free(responses);
            }
            for (api_route.responses.items, 0..) |response, response_idx| {
                const schema_json = if (response.schema_json) |raw_schema|
                    try allocator.dupe(u8, raw_schema)
                else if (response.schema_ref) |schema_ref|
                    if (findSchemaJsonByName(&contract, schema_ref)) |resolved_schema|
                        try allocator.dupe(u8, resolved_schema)
                    else
                        null
                else
                    null;
                errdefer if (schema_json) |owned| allocator.free(owned);

                responses[response_idx] = .{
                    .status = response.status orelse api_route.response_status orelse 200,
                    .content_type = if (response.content_type) |content_type|
                        try allocator.dupe(u8, content_type)
                    else if (api_route.response_content_type) |content_type|
                        try allocator.dupe(u8, content_type)
                    else
                        null,
                    .schema_json = schema_json,
                    .dynamic = response.dynamic,
                };
            }

            routes[out_index] = .{
                .service_name = try allocator.dupe(u8, entry.name),
                .handler_path = try allocator.dupe(u8, entry.path),
                .method = try allocator.dupe(u8, api_route.method),
                .path = try allocator.dupe(u8, api_route.path),
                .required_path_params = try required_path_params.toOwnedSlice(allocator),
                .required_query_params = try required_query_params.toOwnedSlice(allocator),
                .required_header_params = try required_header_params.toOwnedSlice(allocator),
                .request_dynamic = api_route.request_schema_dynamic or api_route.query_params_dynamic or api_route.header_params_dynamic or api_route.request_bodies_dynamic,
                .response_dynamic = api_route.responses_dynamic,
                .requires_body = api_route.request_bodies.items.len > 0,
                .responses = responses,
            };
            out_index += 1;
        }
    }

    return .{ .routes = routes[0..out_index] };
}

fn buildContractForServiceContext(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    sql_schema_path: ?[]const u8,
) !HandlerContract {
    const source = try readFilePosix(allocator, handler_path, 10 * 1024 * 1024);
    defer allocator.free(source);

    var source_to_parse: []const u8 = source;
    var strip_result: ?zigts.StripResult = null;
    defer if (strip_result) |*sr| sr.deinit();

    const is_ts = std.mem.endsWith(u8, handler_path, ".ts");
    const is_tsx = std.mem.endsWith(u8, handler_path, ".tsx");
    if (is_ts or is_tsx) {
        strip_result = try zigts.strip(allocator, source, .{
            .tsx_mode = is_tsx,
            .enable_comptime = true,
            .comptime_env = .{},
        });
        source_to_parse = strip_result.?.code;
    }

    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();

    var js_parser = zigts.parser.JsParser.init(allocator, source_to_parse);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);
    if (std.mem.endsWith(u8, handler_path, ".jsx") or is_tsx) {
        js_parser.tokenizer.enableJsx();
    }

    const root = try js_parser.parse();
    try validateVirtualModuleImports(
        ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants),
        &atoms,
        handler_path,
    );
    _ = zigts.parser.optimizeIR(allocator, &js_parser.nodes, &js_parser.constants, root) catch {};

    return buildContractWithPolicy(
        allocator,
        &js_parser,
        &atoms,
        handler_path,
        root,
        null,
        null,
        if (strip_result) |*sr| &sr.type_map else null,
        null,
        sql_schema_path,
        null,
        null,
    );
}

fn loadServiceTypeContext(
    allocator: std.mem.Allocator,
    system_path: ?[]const u8,
    sql_schema_path: ?[]const u8,
) !?ServiceTypeContext {
    const resolved_system_path = system_path orelse return null;

    const system_json = try readFilePosix(allocator, resolved_system_path, 1024 * 1024);
    defer allocator.free(system_json);

    var config = try system_linker.parseSystemConfig(allocator, system_json);
    defer config.deinit(allocator);
    try resolveSystemHandlerPaths(allocator, resolved_system_path, &config);

    var contracts = try allocator.alloc(HandlerContract, config.handlers.len);
    defer allocator.free(contracts);

    var initialized: usize = 0;
    defer {
        for (contracts[0..initialized]) |*contract| contract.deinit(allocator);
    }

    for (config.handlers, 0..) |entry, idx| {
        contracts[idx] = try buildContractForServiceContext(allocator, entry.path, sql_schema_path);
        initialized += 1;
    }

    return try buildServiceTypeContextFromContracts(allocator, &config, contracts[0..initialized]);
}

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
    sdk_target: ?[]const u8 = null,
    sql_schema_path: ?[]const u8 = null,
    system_path: ?[]const u8 = null,
    policy_path: ?[]const u8 = null,
    deploy_target_str: ?[]const u8 = null,
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

fn parsePrecompileArgs(args_vector: std.process.Args) !PrecompileOptions {
    const allocator = std.heap.smp_allocator;
    const argv = try collectArgs(allocator, args_vector);
    defer {
        for (argv) |arg| allocator.free(arg);
        allocator.free(argv);
    }
    return try parsePrecompileArgSlice(argv[1..]);
}

fn parsePrecompileArgSlice(argv: []const []const u8) !PrecompileOptions {
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
                std.debug.print("Missing target after --sdk (values: ts)\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--sql-schema")) {
            index += 1;
            opts.sql_schema_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --sql-schema\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--system")) {
            index += 1;
            opts.system_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --system\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--policy")) {
            index += 1;
            opts.policy_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --policy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--deploy")) {
            index += 1;
            opts.deploy_target_str = if (index < argv.len) argv[index] else {
                std.debug.print("Missing target after --deploy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--replay")) {
            index += 1;
            opts.replay_trace_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --replay\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--test-file")) {
            index += 1;
            opts.test_file_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --test-file\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--prove")) {
            index += 1;
            opts.prove_spec = if (index < argv.len) argv[index] else {
                std.debug.print("Missing spec after --prove (format: contract.json or contract.json:traces.jsonl)\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--manifest")) {
            index += 1;
            opts.manifest_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --manifest\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--expect-properties")) {
            index += 1;
            opts.expect_properties_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --expect-properties\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--data-labels")) {
            index += 1;
            opts.data_labels_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --data-labels\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--fault-severity")) {
            index += 1;
            opts.fault_severity_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --fault-severity\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--generator-pack")) {
            index += 1;
            opts.generator_pack_path = if (index < argv.len) argv[index] else {
                std.debug.print("Missing path after --generator-pack\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--report")) {
            index += 1;
            opts.report_format = if (index < argv.len) argv[index] else {
                std.debug.print("Missing format after --report (values: json)\n", .{});
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

    const usage = "Usage: precompile [--aot] [--verify] [--contract] [--openapi] [--sdk ts] [--sql-schema path] [--system path] [--prove spec] [--policy policy.json] <handler.ts> <output.zig>\n";
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

fn collectArgs(allocator: std.mem.Allocator, args_vector: std.process.Args) ![]const []const u8 {
    var args_iter = std.process.Args.Iterator.init(args_vector);
    defer args_iter.deinit();

    var args = std.ArrayList([]const u8).empty;
    errdefer args.deinit(allocator);

    while (args_iter.next()) |arg| {
        try args.append(allocator, try allocator.dupe(u8, arg));
    }
    return args.toOwnedSlice(allocator);
}

const ResolvedGeneratorPack = struct {
    sql_schema_path: ?[]const u8 = null,
    manifest_path: ?[]const u8 = null,
    expect_properties_path: ?[]const u8 = null,
    data_labels_path: ?[]const u8 = null,
    replay_trace_path: ?[]const u8 = null,
    fault_severity_path: ?[]const u8 = null,
    report_format: ?[]const u8 = null,

    fn deinit(self: *ResolvedGeneratorPack, allocator: std.mem.Allocator) void {
        const owned = [_]?[]const u8{
            self.sql_schema_path,
            self.manifest_path,
            self.expect_properties_path,
            self.data_labels_path,
            self.replay_trace_path,
            self.fault_severity_path,
            self.report_format,
        };
        for (owned) |value| {
            if (value) |slice| allocator.free(slice);
        }
    }
};

fn dupJsonValue(
    allocator: std.mem.Allocator,
    obj: std.json.ObjectMap,
    key: []const u8,
    resolve_base: ?[]const u8,
) !?[]const u8 {
    const value = obj.get(key) orelse return null;
    if (value != .string) return error.InvalidGeneratorPack;
    if (resolve_base) |base_dir| {
        if (!std.fs.path.isAbsolute(value.string)) {
            return try std.fs.path.resolve(allocator, &.{ base_dir, value.string });
        }
    }
    return try allocator.dupe(u8, value.string);
}

fn resolveGeneratorPack(
    allocator: std.mem.Allocator,
    path: []const u8,
) !ResolvedGeneratorPack {
    const bytes = try readFilePosix(allocator, path, 1024 * 1024);
    defer allocator.free(bytes);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, bytes, .{});
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidGeneratorPack;
    const obj = parsed.value.object;
    const base_dir = std.fs.path.dirname(path) orelse ".";

    var result: ResolvedGeneratorPack = .{};
    errdefer result.deinit(allocator);

    result.sql_schema_path = try dupJsonValue(allocator, obj, "sqlSchema", base_dir);
    result.manifest_path = try dupJsonValue(allocator, obj, "manifest", base_dir);
    result.expect_properties_path = try dupJsonValue(allocator, obj, "expectProperties", base_dir);
    result.data_labels_path = try dupJsonValue(allocator, obj, "dataLabels", base_dir);
    result.replay_trace_path = try dupJsonValue(allocator, obj, "replay", base_dir);
    result.fault_severity_path = try dupJsonValue(allocator, obj, "faultSeverity", base_dir);
    result.report_format = try dupJsonValue(allocator, obj, "report", null);

    return result;
}

pub fn main(init: std.process.Init.Minimal) !void {
    var debug_alloc: if (builtin.mode == .Debug) std.heap.DebugAllocator(.{}) else void =
        if (builtin.mode == .Debug) .init else {};
    defer if (builtin.mode == .Debug) {
        _ = debug_alloc.deinit();
    };
    const allocator = if (builtin.mode == .Debug) debug_alloc.allocator() else std.heap.smp_allocator;

    const argv = try collectArgs(allocator, init.args);
    defer {
        for (argv) |arg| allocator.free(arg);
        allocator.free(argv);
    }

    try runCompileWithArgs(allocator, argv[1..]);
}

pub fn runCompileWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var opts = parsePrecompileArgSlice(argv) catch |err| {
        if (err == error.MissingArgument) return;
        return err;
    };

    var generator_pack: ?ResolvedGeneratorPack = null;
    defer if (generator_pack) |*pack| pack.deinit(allocator);
    if (opts.generator_pack_path) |pack_path| {
        generator_pack = resolveGeneratorPack(allocator, pack_path) catch |err| {
            std.debug.print("Error resolving generator pack '{s}': {}\n", .{ pack_path, err });
            return err;
        };
        if (opts.sql_schema_path == null) opts.sql_schema_path = generator_pack.?.sql_schema_path;
        if (opts.manifest_path == null) opts.manifest_path = generator_pack.?.manifest_path;
        if (opts.expect_properties_path == null) opts.expect_properties_path = generator_pack.?.expect_properties_path;
        if (opts.data_labels_path == null) opts.data_labels_path = generator_pack.?.data_labels_path;
        if (opts.replay_trace_path == null) opts.replay_trace_path = generator_pack.?.replay_trace_path;
        if (opts.fault_severity_path == null) opts.fault_severity_path = generator_pack.?.fault_severity_path;
        if (opts.report_format == null) opts.report_format = generator_pack.?.report_format;
    }

    const handler_path_final = opts.handler_path;
    const output_path_final = opts.output_path;
    const emit_aot = opts.emit_aot;
    const emit_verify = opts.emit_verify;
    const emit_contract = opts.emit_contract;
    const emit_openapi = opts.emit_openapi;
    const sdk_target = opts.sdk_target;
    const sql_schema_path = opts.sql_schema_path;
    const system_path = opts.system_path;
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
        system_path,
    ) catch |err| {
        std.debug.print("Compilation failed: {}\n", .{err});
        return err;
    };
    defer compiled.deinit(allocator);

    // Deferred verification failure: violations enriched with counterexamples, return error now.
    if (compiled.verify_failed) {
        if (compiled.violations_jsonl) |viol_jsonl| {
            const viol_path = deriveSiblingPath(allocator, output_path_final, "handler.violations.jsonl") catch |err| {
                std.debug.print("Error deriving violations output path: {}\n", .{err});
                return error.VerificationFailed;
            };
            defer allocator.free(viol_path);
            writeFilePosix(viol_path, viol_jsonl, allocator) catch |err| {
                std.debug.print("Error writing violations file '{s}': {}\n", .{ viol_path, err });
            };
            if (compiled.violations_summary) |summary| std.debug.print("{s}", .{summary});
            std.debug.print("  Counterexample tests written to: {s}\n", .{viol_path});
            std.debug.print("  Run: zig build run -- {s} --test {s}\n", .{ output_path_final, viol_path });
        } else if (compiled.violations_summary) |summary| {
            std.debug.print("{s}", .{summary});
        }
        return error.VerificationFailed;
    }

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

        const groups = zigts.trace.parseTraceFile(allocator, trace_source) catch |err| {
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

        if (sdk_target) |target| {
            try writeSdkArtifact(allocator, output_path_final, contract, target);
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

        // Write property violation counterexample tests and print unified summary.
        // violations_summary covers all analyzers (flow, verifier, fault coverage).
        // violations_jsonl is only present when fault-coverage counterexamples exist.
        if (compiled.violations_jsonl) |viol_jsonl| {
            const viol_path = deriveSiblingPath(allocator, output_path_final, "handler.violations.jsonl") catch |err| {
                std.debug.print("Error deriving violations output path: {}\n", .{err});
                return err;
            };
            defer allocator.free(viol_path);

            writeFilePosix(viol_path, viol_jsonl, allocator) catch |err| {
                std.debug.print("Error writing violations file '{s}': {}\n", .{ viol_path, err });
                return err;
            };

            if (compiled.violations_summary) |summary| {
                std.debug.print("{s}", .{summary});
            }
            std.debug.print("  Counterexample tests written to: {s}\n", .{viol_path});
            std.debug.print("  Run: zig build run -- {s} --test {s}\n", .{ output_path_final, viol_path });
        } else if (compiled.violations_summary) |summary| {
            // Violations from flow or verifier analysis (no counterexample JSONL).
            std.debug.print("{s}", .{summary});
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

                const groups = zigts.trace.parseTraceFile(allocator, prove_trace_source) catch |err| {
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

            prove_upgrade.writeProofOutputs(allocator, &result, prove_dir) catch |err| {
                std.debug.print("Error writing proof outputs: {}\n", .{err});
                return err;
            };

            std.debug.print("Proven evolution: {s}\n", .{result.certificate.classification.toString()});
        }

        // Manifest alignment check if --manifest was passed
        var manifest_alignment_result: ?manifest_alignment.ManifestAlignment = null;
        defer if (manifest_alignment_result) |*ma| ma.deinit(allocator);

        if (opts.manifest_path) |mpath| {
            const manifest_bytes = readFilePosix(allocator, mpath, 1024 * 1024) catch |err| {
                std.debug.print("Error reading manifest file '{s}': {}\n", .{ mpath, err });
                return err;
            };
            defer allocator.free(manifest_bytes);

            var parsed_manifest = manifest_alignment.parseManifest(allocator, manifest_bytes) catch |err| {
                std.debug.print("Error parsing manifest '{s}': {}\n", .{ mpath, err });
                return err;
            };
            defer parsed_manifest.deinit(allocator);

            manifest_alignment_result = manifest_alignment.checkAlignment(allocator, &parsed_manifest, contract) catch |err| {
                std.debug.print("Error checking manifest alignment: {}\n", .{err});
                return err;
            };

            manifest_alignment.printAlignmentSummary(&manifest_alignment_result.?);
        }

        // Property expectations check if --expect-properties was passed
        var property_result: ?property_expectations.ExpectationResult = null;
        defer if (property_result) |pr| allocator.free(pr.mismatches);

        if (opts.expect_properties_path) |epath| {
            property_result = property_expectations.checkExpectationsFromFile(allocator, epath, contract) catch |err| {
                std.debug.print("Error checking property expectations: {}\n", .{err});
                return err;
            };

            property_expectations.printExpectationResults(&property_result.?);
        }

        // Structured report output if --report was passed
        if (opts.report_format) |fmt| {
            if (std.mem.eql(u8, fmt, "json")) {
                const integration_inputs = build_report.IntegrationSection{
                    .generator_pack = opts.generator_pack_path != null,
                    .sql_schema = opts.sql_schema_path != null,
                    .manifest = opts.manifest_path != null,
                    .property_expectations = opts.expect_properties_path != null,
                    .data_labels = opts.data_labels_path != null,
                    .replay = opts.replay_trace_path != null,
                    .fault_severity = opts.fault_severity_path != null,
                };
                var handler_report = build_report.buildReport(
                    allocator,
                    contract,
                    if (manifest_alignment_result) |*ma| ma else null,
                    if (property_result) |*pr| pr else null,
                    &integration_inputs,
                    handler_path_final,
                );
                defer {
                    if (handler_report.verification) |v| allocator.free(v.checks);
                    if (handler_report.manifest_alignment) |ma| allocator.free(ma.sections);
                }

                const report_path = deriveSiblingPath(allocator, output_path_final, "report.json") catch |err| {
                    std.debug.print("Error deriving report path: {}\n", .{err});
                    return err;
                };
                defer allocator.free(report_path);

                var report_output: std.ArrayList(u8) = .empty;
                defer report_output.deinit(allocator);
                var report_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &report_output);

                build_report.writeReportJson(&report_aw.writer, &handler_report) catch |err| {
                    std.debug.print("Error serializing report: {}\n", .{err});
                    return err;
                };
                report_output = report_aw.toArrayList();

                writeFilePosix(report_path, report_output.items, allocator) catch |err| {
                    std.debug.print("Error writing report '{s}': {}\n", .{ report_path, err });
                    return err;
                };

                std.debug.print("Wrote build report to: {s}\n", .{report_path});
            } else {
                std.debug.print("Unknown report format: {s} (supported: json)\n", .{fmt});
            }
        }
    }
}

pub const CheckResult = struct {
    line_count: u32 = 0,
    parse_errors: u32 = 0,
    bool_specializations: u32 = 0,
    bool_errors: u32 = 0,
    bool_warnings: u32 = 0,
    type_errors: u32 = 0,
    is_typescript: bool = false,
    verify_ran: bool = false,
    verify_errors: u32 = 0,
    verify_warnings: u32 = 0,
    exhaustive_returns: bool = false,
    results_safe: bool = false,
    optionals_safe: bool = false,
    state_isolated: bool = true,
    no_unreachable: bool = true,
    paths_enumerated: u32 = 0,
    paths_exhaustive: bool = false,
    max_io_depth: ?u32 = null,
    fault_total: u32 = 0,
    fault_covered: u32 = 0,
    properties: ?handler_contract.HandlerProperties = null,
    contract: ?HandlerContract = null,
    /// Structured diagnostics for JSON output mode.
    json_diagnostics: std.ArrayList(json_diag.JsonDiagnostic) = .empty,

    pub fn totalErrors(self: *const CheckResult) u32 {
        return self.parse_errors + self.bool_errors + self.type_errors + self.verify_errors;
    }

    pub fn totalWarnings(self: *const CheckResult) u32 {
        return self.bool_warnings + self.verify_warnings;
    }

    pub fn deinit(self: *CheckResult, allocator: std.mem.Allocator) void {
        if (self.contract) |*c| c.deinit(allocator);
        self.json_diagnostics.deinit(allocator);
    }
};

/// Run the full analysis pipeline without generating bytecode.
/// Returns a CheckResult with all verification, contract, and coverage data.
pub fn runCheckOnly(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
    sql_schema_path: ?[]const u8,
    json_mode: bool,
    system_path: ?[]const u8,
) !CheckResult {
    const source = readFilePosix(allocator, handler_path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading handler file '{s}': {}\n", .{ handler_path, err });
        return err;
    };
    defer allocator.free(source);
    return runCheckOnlyFromSource(allocator, source, handler_path, sql_schema_path, json_mode, system_path, false);
}

/// Like runCheckOnly but operates on pre-read source. When skip_contract
/// is true, stages 7-10 (verification, contract, paths, fault coverage)
/// are skipped - only parse and type checking run, enough to detect errors.
pub fn runCheckOnlyFromSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    handler_path: []const u8,
    sql_schema_path: ?[]const u8,
    json_mode: bool,
    system_path: ?[]const u8,
    skip_contract: bool,
) !CheckResult {
    var result = CheckResult{};
    result.line_count = @intCast(std.mem.count(u8, source, "\n") + 1);

    var source_to_parse: []const u8 = source;
    var strip_result: ?zigts.StripResult = null;
    defer if (strip_result) |*sr| sr.deinit();

    const is_ts = std.mem.endsWith(u8, handler_path, ".ts");
    const is_tsx = std.mem.endsWith(u8, handler_path, ".tsx");
    result.is_typescript = is_ts or is_tsx;

    var service_type_context = try loadServiceTypeContext(allocator, system_path, sql_schema_path);
    defer if (service_type_context) |*ctx| ctx.deinit(allocator);
    const stc_ptr: ?*const ServiceTypeContext = if (service_type_context) |*ctx| ctx else null;

    // Stage 1: TypeScript strip
    if (is_ts or is_tsx) {
        strip_result = zigts.strip(allocator, source, .{
            .tsx_mode = is_tsx,
            .enable_comptime = true,
            .comptime_env = .{},
        }) catch |err| {
            std.debug.print("TypeScript strip error: {}\n", .{err});
            result.parse_errors = 1;
            return result;
        };
        source_to_parse = strip_result.?.code;
    }

    // Stage 2: Parse
    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();

    var js_parser = zigts.parser.JsParser.init(allocator, source_to_parse);
    defer js_parser.deinit();
    js_parser.setAtomTable(&atoms);
    if (std.mem.endsWith(u8, handler_path, ".jsx") or is_tsx) {
        js_parser.tokenizer.enableJsx();
    }

    const root = js_parser.parse() catch {
        const errors = js_parser.errors.getErrors();
        if (json_mode) {
            for (errors) |parse_error| {
                result.json_diagnostics.append(allocator, json_diag.fromParseError(parse_error, handler_path)) catch {};
            }
        } else {
            for (errors) |parse_error| {
                std.debug.print("{s}:{}:{}: {s}\n", .{
                    handler_path,
                    parse_error.location.line,
                    parse_error.location.column,
                    parse_error.message,
                });
            }
        }
        result.parse_errors = @intCast(errors.len);
        return result;
    };

    // Stage 3: Import validation
    validateVirtualModuleImports(
        ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants),
        &atoms,
        handler_path,
    ) catch {
        result.parse_errors = 1;
        return result;
    };

    // Stage 4: IR optimization
    _ = zigts.parser.optimizeIR(
        allocator,
        &js_parser.nodes,
        &js_parser.constants,
        root,
    ) catch {};

    // Stage 5: Bool checker (sound mode)
    {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        var checker = zigts.BoolChecker.init(allocator, ir_view, &atoms);
        defer checker.deinit();

        result.bool_errors = @intCast(try checker.check(root));
        const bool_diags = checker.getDiagnostics();
        result.bool_warnings = @intCast(bool_diags.len -| result.bool_errors);

        if (bool_diags.len > 0) {
            if (json_mode) {
                for (bool_diags) |diag| {
                    if (json_diag.fromBoolDiagnostic(diag, ir_view, handler_path)) |jd| {
                        result.json_diagnostics.append(allocator, jd) catch {};
                    }
                }
            } else {
                var buf: std.ArrayList(u8) = .empty;
                defer buf.deinit(allocator);
                var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
                checker.formatDiagnostics(source_to_parse, &aw.writer) catch {};
                buf = aw.toArrayList();
                if (buf.items.len > 0) std.debug.print("{s}", .{buf.items});
            }
        }

        if (result.bool_errors > 0) {
            return result;
        }

        result.bool_specializations = checker.node_types.count();
    }

    // Stage 6: Type checker (TS only)
    if (strip_result) |sr| {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const tm = sr.type_map;
        var type_pool = zigts.TypePool.init(allocator);
        defer type_pool.deinit(allocator);
        var type_env = zigts.TypeEnv.init(allocator, &type_pool);
        defer type_env.deinit();
        zigts.modules.populateModuleTypes(&type_env, &type_pool, allocator);
        type_env.populateFromTypeMap(&tm);

        var tc = zigts.type_checker.TypeChecker.init(
            allocator,
            ir_view,
            &atoms,
            &type_env,
            stc_ptr,
        );
        defer tc.deinit();
        result.type_errors = @intCast(try tc.check(root));
        const tc_diags = tc.getDiagnostics();
        if (tc_diags.len > 0) {
            if (json_mode) {
                for (tc_diags) |diag| {
                    if (json_diag.fromTypeDiagnostic(diag, ir_view, handler_path)) |jd| {
                        result.json_diagnostics.append(allocator, jd) catch {};
                    }
                }
            } else {
                var buf: std.ArrayList(u8) = .empty;
                defer buf.deinit(allocator);
                var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
                tc.formatDiagnostics(source_to_parse, &aw.writer) catch {};
                buf = aw.toArrayList();
                if (buf.items.len > 0) std.debug.print("{s}", .{buf.items});
            }
        }
        if (result.type_errors > 0) {
            return result;
        }
    }

    if (skip_contract) return result;

    // Stage 7: Handler verification (7 checks)
    var verify_info: ?VerificationInfo = null;
    {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = zigts.handler_verifier.findHandlerFunction(ir_view, root);

        var verify_type_pool: ?zigts.TypePool = null;
        var verify_type_env: ?zigts.TypeEnv = null;
        var verify_type_checker: ?zigts.TypeChecker = null;
        defer if (verify_type_checker) |*c| c.deinit();
        defer if (verify_type_env) |*e| e.deinit();
        defer if (verify_type_pool) |*p| p.deinit(allocator);

        const verifier_env: ?*const zigts.TypeEnv = if (strip_result) |sr| blk: {
            verify_type_pool = zigts.TypePool.init(allocator);
            verify_type_env = zigts.TypeEnv.init(allocator, &verify_type_pool.?);
            zigts.modules.populateModuleTypes(&verify_type_env.?, &verify_type_pool.?, allocator);
            verify_type_env.?.populateFromTypeMap(&sr.type_map);
            verify_type_checker = zigts.TypeChecker.init(
                allocator,
                ir_view,
                &atoms,
                &verify_type_env.?,
                stc_ptr,
            );
            _ = verify_type_checker.?.check(root) catch 0;
            break :blk &verify_type_env.?;
        } else null;

        const verifier_tc: ?*const zigts.TypeChecker = if (verify_type_checker) |*c| c else null;

        if (handler_fn) |hf| {
            result.verify_ran = true;
            var verifier = zigts.HandlerVerifier.init(allocator, ir_view, &atoms, verifier_env, verifier_tc);
            defer verifier.deinit();

            result.verify_errors = @intCast(try verifier.verify(hf));
            const diags = verifier.getDiagnostics();
            result.verify_warnings = @intCast(diags.len -| result.verify_errors);

            if (diags.len > 0) {
                if (json_mode) {
                    for (diags) |diag| {
                        if (json_diag.fromVerifierDiagnostic(diag, ir_view, handler_path)) |jd| {
                            result.json_diagnostics.append(allocator, jd) catch {};
                        }
                    }
                } else {
                    var buf: std.ArrayList(u8) = .empty;
                    defer buf.deinit(allocator);
                    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
                    verifier.formatDiagnostics(source_to_parse, &aw.writer) catch {};
                    buf = aw.toArrayList();
                    if (buf.items.len > 0) std.debug.print("{s}", .{buf.items});
                }
            }

            if (result.verify_errors == 0) {
                result.exhaustive_returns = true;
                result.results_safe = true;
                result.optionals_safe = true;
                result.state_isolated = !verifier.has_module_mutation;
                verify_info = .{
                    .exhaustive_returns = true,
                    .results_safe = true,
                    .unreachable_code = false,
                    .bytecode_verified = false,
                };
                // Check for unreachable code warnings
                for (diags) |d| {
                    if (d.kind == .unreachable_after_return) {
                        result.no_unreachable = false;
                        break;
                    }
                }
            }
        }
    }

    // Stage 8: Contract + flow analysis
    result.contract = try buildContractWithPolicy(
        allocator,
        &js_parser,
        &atoms,
        handler_path,
        root,
        null,
        verify_info,
        if (strip_result) |*sr| &sr.type_map else null,
        null,
        sql_schema_path,
        null,
        stc_ptr,
    );

    if (result.contract) |*c| {
        if (c.properties) |*props| {
            props.state_isolated = result.state_isolated;
            props.result_safe = result.results_safe;
            props.optional_safe = result.optionals_safe;
        }
        result.properties = c.properties;
    }

    // Stage 9: Path generation + Stage 10: Fault coverage
    {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = findHandlerFunction(ir_view, root);
        if (handler_fn) |hf| {
            var gen = zigts.PathGenerator.init(allocator, ir_view, &atoms);
            defer gen.deinit();
            try gen.generate(hf);

            const tests = gen.getTests();
            result.paths_enumerated = @intCast(tests.len);
            result.paths_exhaustive = tests.len < zigts.PathGenerator.MAX_PATHS;

            // Max I/O depth
            var max_depth: u32 = 0;
            for (tests) |t| {
                const depth: u32 = @intCast(t.io_stubs.items.len);
                if (depth > max_depth) max_depth = depth;
            }
            result.max_io_depth = if (tests.len > 0) max_depth else null;

            // Populate behavioral contract
            if (result.contract) |*c| {
                if (c.properties) |*props| {
                    props.max_io_depth = result.max_io_depth;
                }
                c.behaviors = try gen.toBehaviorPaths(allocator);
                c.behaviors_exhaustive = result.paths_exhaustive;
            }

            // Fault coverage
            var fc = zigts.fault_coverage.FaultCoverageChecker.init(allocator, tests);
            defer fc.deinit();
            try fc.analyze();
            const fc_report = fc.getReport();
            result.fault_total = fc_report.total_failable;
            result.fault_covered = fc_report.covered;

            if (result.contract) |*c| {
                c.fault_coverage = .{
                    .total_failable = fc_report.total_failable,
                    .covered = fc_report.covered,
                    .warnings = fc_report.warning_count,
                };
                if (c.properties) |*props| {
                    props.fault_covered = fc_report.isClean();
                }
            }
        }
    }

    return result;
}

/// Format a structured proof card showing what the compiler proved.
pub fn formatProofCard(writer: anytype, r: *const CheckResult, filename: []const u8) void {
    writer.print("\nzts check: {s}\n\n", .{filename}) catch return;

    // Parse
    writeDotted(writer, "Parse", 24);
    if (r.parse_errors > 0) {
        writer.print("FAIL ({d} errors)\n", .{r.parse_errors}) catch return;
    } else {
        writer.print("OK ({d} lines)\n", .{r.line_count}) catch return;
    }

    // Types
    if (r.is_typescript) {
        writeDotted(writer, "Types", 24);
        if (r.type_errors > 0) {
            writer.print("FAIL ({d} errors)\n", .{r.type_errors}) catch return;
        } else {
            writer.print("OK\n", .{}) catch return;
        }
    }

    // Sound mode
    writeDotted(writer, "Sound mode", 24);
    if (r.bool_errors > 0) {
        writer.print("FAIL ({d} errors)\n", .{r.bool_errors}) catch return;
    } else if (r.bool_specializations > 0) {
        writer.print("OK ({d} specializations)\n", .{r.bool_specializations}) catch return;
    } else {
        writer.print("OK\n", .{}) catch return;
    }

    // Verification
    if (r.verify_ran) {
        writer.print("\n  Verification:\n", .{}) catch return;
        writeProven(writer, "exhaustive_returns", r.verify_errors == 0 and r.exhaustive_returns);
        writeProven(writer, "results_safe", r.results_safe);
        writeProven(writer, "optionals_safe", r.optionals_safe);
        writeProven(writer, "state_isolated", r.state_isolated);
        writeProven(writer, "no_unreachable", r.no_unreachable);
    }

    // Properties
    if (r.properties) |props| {
        writer.print("\n  Properties:\n", .{}) catch return;
        writeProven(writer, "retry_safe", props.retry_safe);
        writeProven(writer, "idempotent", props.idempotent);
        writeProven(writer, "injection_safe", props.injection_safe);
        writeProven(writer, "deterministic", props.deterministic);
        writeProven(writer, "read_only", props.read_only);

        writer.print("\n  Security:\n", .{}) catch return;
        writeProven(writer, "no_secret_leakage", props.no_secret_leakage);
        writeProven(writer, "no_credential_leak", props.no_credential_leakage);
        writeProven(writer, "input_validated", props.input_validated);
    }

    // Summary stats
    writer.print("\n", .{}) catch return;
    if (r.fault_total > 0) {
        writer.print("  Fault coverage: {d}/{d} paths covered\n", .{ r.fault_covered, r.fault_total }) catch return;
    }
    if (r.paths_enumerated > 0) {
        writer.print("  Execution paths: {d}", .{r.paths_enumerated}) catch return;
        if (r.paths_exhaustive) {
            writer.print(" (exhaustive)\n", .{}) catch return;
        } else {
            writer.print(" (limit reached)\n", .{}) catch return;
        }
    }
    if (r.max_io_depth) |depth| {
        writer.print("  Max I/O depth: {d}\n", .{depth}) catch return;
    }

    writer.print("\n  {d} errors, {d} warnings\n", .{ r.totalErrors(), r.totalWarnings() }) catch return;
}

const dots = "." ** 32;

fn writeDotted(writer: anytype, label: []const u8, width: usize) void {
    writer.print("  {s} ", .{label}) catch return;
    const pad = @min(width -| (label.len + 1), dots.len);
    writer.writeAll(dots[0..pad]) catch return;
    writer.writeAll(" ") catch return;
}

fn writeProven(writer: anytype, label: []const u8, proven: bool) void {
    writer.print("    {s} ", .{label}) catch return;
    const pad = @min(20 -| label.len, dots.len);
    writer.writeAll(dots[0..pad]) catch return;
    writer.writeAll(if (proven) " PROVEN\n" else " ---\n") catch return;
}

/// Generate TypeScript type definitions for all virtual modules.
pub fn generateTypeDefs(writer: anytype) void {
    writer.print("// Generated by: zigts check --types\n// Do not edit manually.\n\n", .{}) catch return;

    // Request and Response globals
    writer.print(
        \\interface RequestInit {{
        \\  method?: string;
        \\  headers?: Record<string, string>;
        \\  body?: string;
        \\}}
        \\
        \\interface ResponseInit {{
        \\  status?: number;
        \\  statusText?: string;
        \\  headers?: Record<string, string>;
        \\}}
        \\
        \\declare class Request {{
        \\  readonly method: string;
        \\  readonly url: string;
        \\  readonly path: string;
        \\  readonly query: string;
        \\  readonly body: string | undefined;
        \\  readonly headers: Record<string, string>;
        \\}}
        \\
        \\declare class Response {{
        \\  readonly body: string;
        \\  readonly status: number;
        \\  readonly statusText: string;
        \\  readonly ok: boolean;
        \\  readonly headers: Record<string, string>;
        \\  static json(data: unknown, init?: ResponseInit): Response;
        \\  static text(text: string, init?: ResponseInit): Response;
        \\  static html(html: string, init?: ResponseInit): Response;
        \\  static redirect(url: string, status?: number): Response;
        \\}}
        \\
        \\
    , .{}) catch return;

    const modules = @import("zigts").builtin_modules;
    for (modules.all) |binding| {
        writer.print("declare module \"{s}\" {{\n", .{binding.specifier}) catch return;
        for (binding.exports) |func| {
            writer.print("  export function {s}(", .{func.name}) catch return;
            for (func.param_types, 0..) |pt, i| {
                if (i > 0) writer.print(", ", .{}) catch return;
                writer.print("arg{d}: {s}", .{ i, returnKindToTs(pt) }) catch return;
            }
            writer.print("): {s};\n", .{returnKindToTs(func.returns)}) catch return;
        }
        writer.print("}}\n\n", .{}) catch return;
    }
}

fn returnKindToTs(kind: @import("zigts").module_binding.ReturnKind) []const u8 {
    return switch (kind) {
        .boolean => "boolean",
        .number => "number",
        .string => "string",
        .object => "Record<string, unknown>",
        .undefined => "void",
        .unknown => "unknown",
        .optional_string => "string | undefined",
        .optional_object => "Record<string, unknown> | undefined",
        .result => "{ ok: boolean; value?: unknown; error?: string; errors?: unknown }",
    };
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
/// Creates a fresh zigts context per trace for isolation (the interpreter mutates
/// context state during execution). This is O(N*parse) but acceptable at build
/// time since trace counts are typically small.
fn runBuildTimeReplay(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    groups: []const zigts.trace.RequestTraceGroup,
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
    request: zigts.trace.RequestTrace,
    io_calls: []const zigts.trace.IoEntry,
) !HandlerExecResult {
    const ctx = try zigts.createContext(allocator, .{ .nursery_size = 64 * 1024 });
    defer zigts.destroyContext(ctx);
    try zigts.builtins.initBuiltins(ctx);

    inline for (zigts.builtin_modules.all) |binding| {
        try zigts.modules.registerVirtualModuleReplay(binding, ctx, allocator);
    }

    var replay_state = zigts.trace.ReplayState{
        .io_calls = io_calls,
        .cursor = 0,
        .divergences = 0,
    };
    ctx.setModuleState(
        zigts.trace.REPLAY_STATE_SLOT,
        @ptrCast(&replay_state),
        &zigts.trace.ReplayState.deinitOpaque,
    );
    defer ctx.module_state[zigts.trace.REPLAY_STATE_SLOT] = null;

    var strings = zigts.StringTable.init(allocator);
    defer strings.deinit();

    var source_to_parse: []const u8 = handler_source;
    var strip_result: ?zigts.StripResult = null;
    defer if (strip_result) |*sr| sr.deinit();

    const is_ts = std.mem.endsWith(u8, handler_filename, ".ts");
    const is_tsx = std.mem.endsWith(u8, handler_filename, ".tsx");
    if (is_ts or is_tsx) {
        strip_result = zigts.strip(allocator, handler_source, .{ .tsx_mode = is_tsx }) catch return error.StripFailed;
        source_to_parse = strip_result.?.code;
    }

    var p = zigts.Parser.init(allocator, source_to_parse, &strings, &ctx.atoms);
    defer p.deinit();
    if (std.mem.endsWith(u8, handler_filename, ".jsx") or is_tsx) {
        p.enableJsx();
    }

    const bytecode_data = p.parse() catch return error.ParseFailed;

    const shapes = p.getShapes();
    if (shapes.len > 0) {
        try ctx.materializeShapes(shapes);
    }

    const func = zigts.bytecode.FunctionBytecode{
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

    var interp = zigts.Interpreter.init(ctx);
    _ = try interp.run(&func);

    const handler_val = ctx.getGlobal(zigts.Atom.handler) orelse return error.NoHandler;
    if (!handler_val.isObject()) return error.NoHandler;
    const handler_obj = zigts.JSObject.fromValue(handler_val);
    if (handler_obj.class_id != .function or !handler_obj.flags.is_callable) return error.NoHandler;

    const hc_pool_req = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;
    const req_obj = try ctx.createObject(null);

    const method_val = try ctx.createString(request.method);
    try req_obj.setProperty(allocator, hc_pool_req, zigts.Atom.method, method_val);

    const url_val = try ctx.createString(request.url);
    try req_obj.setProperty(allocator, hc_pool_req, zigts.Atom.url, url_val);

    if (request.body) |body| {
        const body_val = try ctx.createString(body);
        try req_obj.setProperty(allocator, hc_pool_req, zigts.Atom.body, body_val);
    }

    const args = [_]zigts.JSValue{req_obj.toValue()};
    const bc_data = handler_obj.getBytecodeFunctionData() orelse return error.NotCallable;
    const result = interp.callBytecodeFunction(
        handler_obj.toValue(),
        bc_data.bytecode,
        zigts.JSValue.undefined_val,
        &args,
    ) catch return error.HandlerError;

    if (!result.isObject()) return error.InvalidResponse;
    const result_obj = zigts.JSObject.fromValue(result);
    const hc_pool = ctx.hidden_class_pool orelse return error.NoHiddenClassPool;

    const status_val = result_obj.getProperty(hc_pool, zigts.Atom.status) orelse zigts.JSValue.fromInt(200);
    const actual_status: u16 = if (status_val.isInt())
        @intCast(@max(0, @min(999, status_val.getInt())))
    else
        200;

    const body_val = result_obj.getProperty(hc_pool, zigts.Atom.body) orelse zigts.JSValue.undefined_val;

    return .{
        .status = actual_status,
        .body = zigts.trace.extractStringData(body_val) orelse "",
        .divergences = replay_state.divergences,
    };
}

/// Replay a single trace at build time.
fn replayOneBuildTime(
    allocator: std.mem.Allocator,
    handler_source: []const u8,
    handler_filename: []const u8,
    group: *const zigts.trace.RequestTraceGroup,
) !bool {
    const r = try executeBuildTimeHandler(allocator, handler_source, handler_filename, group.request, group.io_calls);

    const expected = group.response orelse return true;
    const status_ok = r.status == expected.status;

    // Unescape before comparing (trace stores JSON-escaped body strings).
    const expected_body = zigts.trace.unescapeJson(allocator, expected.body) catch expected.body;
    defer if (expected_body.ptr != expected.body.ptr) allocator.free(expected_body);

    return status_ok and std.mem.eql(u8, r.body, expected_body) and r.divergences == 0;
}

// ============================================================================
// Build-Time Handler Tests
// ============================================================================

const BuildTestCase = struct {
    name: []const u8,
    request: ?zigts.trace.RequestTrace = null,
    io_calls: []const zigts.trace.IoEntry = &.{},
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
            const unescaped = zigts.trace.unescapeJson(allocator, expected) catch expected;
            defer if (unescaped.ptr != expected.ptr) allocator.free(unescaped);
            if (!std.mem.eql(u8, r.body, unescaped)) {
                std.debug.print("  FAIL  {s}\n        body mismatch\n        expected: {s}\n        actual:   {s}\n", .{
                    tc.name, truncate(unescaped, 200), truncate(r.body, 200),
                });
                ok = false;
            }
        }
        if (tc.expected_body_contains) |needle| {
            const unescaped = zigts.trace.unescapeJson(allocator, needle) catch needle;
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
    var current_request: ?zigts.trace.RequestTrace = null;
    var current_io: std.ArrayList(zigts.trace.IoEntry) = .empty;
    defer current_io.deinit(allocator);
    var current_status: ?u16 = null;
    var current_body: ?[]const u8 = null;
    var current_body_contains: ?[]const u8 = null;

    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        const type_str = zigts.trace.findJsonStringValue(line, "\"type\"") orelse continue;

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
            current_name = zigts.trace.findJsonStringValue(line, "\"name\"") orelse "unnamed";
        } else if (std.mem.eql(u8, type_str, "request")) {
            current_request = .{
                .method = zigts.trace.findJsonStringValue(line, "\"method\"") orelse "GET",
                .url = zigts.trace.findJsonStringValue(line, "\"url\"") orelse "/",
                .headers_json = zigts.trace.findJsonObjectValue(line, "\"headers\"") orelse "{}",
                .body = zigts.trace.findJsonStringValue(line, "\"body\""),
            };
        } else if (std.mem.eql(u8, type_str, "io")) {
            try current_io.append(allocator, .{
                .seq = @intCast(zigts.trace.findJsonIntValue(line, "\"seq\"") orelse 0),
                .module = zigts.trace.findJsonStringValue(line, "\"module\"") orelse "",
                .func = zigts.trace.findJsonStringValue(line, "\"fn\"") orelse "",
                .args_json = zigts.trace.findJsonArrayValue(line, "\"args\"") orelse "[]",
                .result_json = zigts.trace.findJsonAnyValue(line, "\"result\"") orelse "null",
            });
        } else if (std.mem.eql(u8, type_str, "expect")) {
            const status_val = zigts.trace.findJsonIntValue(line, "\"status\"");
            current_status = if (status_val) |s| @intCast(@max(0, @min(999, s))) else null;
            current_body = zigts.trace.findJsonStringValue(line, "\"body\"");
            current_body_contains = zigts.trace.findJsonStringValue(line, "\"bodyContains\"");
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

pub fn compileHandler(
    allocator: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    emit_aot: bool,
    emit_verify: bool,
    emit_contract: bool,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
    generate_tests: bool,
    system_path: ?[]const u8,
) !CompiledHandler {
    var source_to_parse: []const u8 = source;
    var strip_result: ?zigts.StripResult = null;
    defer if (strip_result) |*sr| sr.deinit();

    // Type strip for .ts/.tsx files
    const is_ts = std.mem.endsWith(u8, filename, ".ts");
    const is_tsx = std.mem.endsWith(u8, filename, ".tsx");

    if (is_ts or is_tsx) {
        // Build comptime environment with build metadata
        const comptime_env = zigts.ComptimeEnv{
            .build_time = null, // TODO: pass actual build time
            .git_commit = null, // TODO: pass git commit
            .version = zigts.version.string,
            .env_vars = null,
        };

        strip_result = zigts.strip(allocator, source, .{
            .tsx_mode = is_tsx,
            .enable_comptime = true,
            .comptime_env = comptime_env,
        }) catch |err| {
            std.debug.print("TypeScript strip error: {}\n", .{err});
            return err;
        };
        source_to_parse = strip_result.?.code;
        if (!builtin.is_test) std.debug.print("TypeScript stripped successfully\n", .{});
    }

    // Initialize string table and atom table for parsing
    var strings = zigts.StringTable.init(allocator);
    defer strings.deinit();

    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();

    // Parse the source code (single pass for IR + bytecode)
    var js_parser = zigts.parser.JsParser.init(allocator, source_to_parse);
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

    try validateVirtualModuleImports(
        ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants),
        &atoms,
        filename,
    );

    // Check for file imports before proceeding with single-module compilation
    const has_file_imports = hasFileImports(&js_parser, root);

    var service_type_context = try loadServiceTypeContext(allocator, system_path, sql_schema_path);
    defer if (service_type_context) |*ctx| ctx.deinit(allocator);
    const stc_ptr: ?*const ServiceTypeContext = if (service_type_context) |*ctx| ctx else null;

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
            stc_ptr,
        );
    }

    // Optimize IR (cold-start-friendly single pass)
    _ = zigts.parser.optimizeIR(
        allocator,
        &js_parser.nodes,
        &js_parser.constants,
        root,
    ) catch {};

    // Node type map for type-directed codegen (populated by BoolChecker, consumed by CodeGen)
    var node_type_map: zigts.bool_checker.NodeTypeMap = .empty;
    defer node_type_map.deinit(allocator);

    // Run bool checker and type checker
    {
        const ir_view_check = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        var checker = zigts.BoolChecker.init(allocator, ir_view_check, &atoms);
        defer checker.deinit();

        const bool_error_count = try checker.check(root);
        const bool_diags = checker.getDiagnostics();

        if (bool_diags.len > 0 and !builtin.is_test) {
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
            if (!builtin.is_test) std.debug.print("\nBoolean check failed for {s}\n", .{filename});
            return error.SoundModeViolation;
        }

        if (!builtin.is_test) std.debug.print("Boolean check passed\n", .{});

        // Extract node type map for codegen specialization (move ownership out of checker)
        node_type_map = checker.node_types;
        checker.node_types = .empty; // Prevent double-free in checker.deinit()

        // TypeChecker: full type annotation checking (when TypeMap available from .ts/.tsx)
        if (strip_result) |sr| {
            const tm = sr.type_map;
            var type_pool = zigts.TypePool.init(allocator);
            defer type_pool.deinit(allocator);

            var type_env = zigts.TypeEnv.init(allocator, &type_pool);
            defer type_env.deinit();

            zigts.modules.populateModuleTypes(&type_env, &type_pool, allocator);
            type_env.populateFromTypeMap(&tm);

            var tc = zigts.type_checker.TypeChecker.init(
                allocator,
                ir_view_check,
                &atoms,
                &type_env,
                stc_ptr,
            );
            defer tc.deinit();

            const tc_errors = try tc.check(root);
            const tc_diags = tc.getDiagnostics();

            if (tc_diags.len > 0 and !builtin.is_test) {
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
                if (!builtin.is_test) std.debug.print("\nType check failed for {s}\n", .{filename});
                return error.SoundModeViolation;
            }
            if (!builtin.is_test) std.debug.print("Type check passed\n", .{});
        }
    }

    // Unified violations list: collects from FlowChecker, HandlerVerifier, and
    // FaultCoverageChecker. Declared here so it spans the verifier block,
    // buildContractWithPolicy (flow violations), and the PathGenerator block (fault violations).
    var all_violations: std.ArrayList(zigts.property_diagnostics.PropertyViolation) = .empty;
    defer {
        zigts.property_diagnostics.deinitViolations(allocator, all_violations.items);
        all_violations.deinit(allocator);
    }

    // Run handler verification if requested
    var verify_info: ?VerificationInfo = null;
    var state_isolated: bool = true;
    // result_safe and optional_safe default false (unproven) until verification runs and passes.
    var result_safe: bool = false;
    var optional_safe: bool = false;
    if (emit_verify) {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = zigts.handler_verifier.findHandlerFunction(ir_view, root);

        var verify_type_pool: ?zigts.TypePool = null;
        var verify_type_env: ?zigts.TypeEnv = null;
        var verify_type_checker: ?zigts.TypeChecker = null;
        defer if (verify_type_checker) |*checker| checker.deinit();
        defer if (verify_type_env) |*env| env.deinit();
        defer if (verify_type_pool) |*pool| pool.deinit(allocator);

        const verifier_env: ?*const zigts.TypeEnv = if (strip_result) |sr| blk: {
            verify_type_pool = zigts.TypePool.init(allocator);
            verify_type_env = zigts.TypeEnv.init(allocator, &verify_type_pool.?);
            zigts.modules.populateModuleTypes(&verify_type_env.?, &verify_type_pool.?, allocator);
            verify_type_env.?.populateFromTypeMap(&sr.type_map);
            verify_type_checker = zigts.TypeChecker.init(
                allocator,
                ir_view,
                &atoms,
                &verify_type_env.?,
                stc_ptr,
            );
            _ = verify_type_checker.?.check(root) catch 0;
            break :blk &verify_type_env.?;
        } else null;

        const verifier_type_checker: ?*const zigts.TypeChecker = if (verify_type_checker) |*checker| checker else null;

        if (handler_fn) |hf| {
            var verifier = zigts.HandlerVerifier.init(allocator, ir_view, &atoms, verifier_env, verifier_type_checker);
            defer verifier.deinit();

            const error_count = try verifier.verify(hf);
            const diags = verifier.getDiagnostics();

            if (diags.len > 0 and !builtin.is_test) {
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

            // Collect violations before deciding to fail so counterexample tests can be
            // generated and returned even when verification fails.
            zigts.property_diagnostics.collectVerifierViolations(allocator, &all_violations, diags, ir_view);

            if (error_count > 0) {
                // Run PathGenerator to fill counterexample refs for result_unsafe violations
                // before returning the failure result.
                var gen = zigts.PathGenerator.init(allocator, ir_view, &atoms);
                defer gen.deinit();
                gen.generate(hf) catch {};
                zigts.property_diagnostics.fillVerifierCounterexamples(all_violations.items, gen.getTests());

                var viol_jsonl_result: ?[]const u8 = null;
                var viol_summary_result: ?[]const u8 = null;
                if (all_violations.items.len > 0) {
                    var vj_buf: std.ArrayList(u8) = .empty;
                    var vj_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &vj_buf);
                    zigts.property_diagnostics.writeViolationsJsonl(&vj_aw.writer, allocator, all_violations.items, gen.getTests()) catch {};
                    vj_buf = vj_aw.toArrayList();
                    if (vj_buf.items.len > 0) {
                        viol_jsonl_result = try vj_buf.toOwnedSlice(allocator);
                    } else {
                        vj_buf.deinit(allocator);
                    }
                    var vs_buf: std.ArrayList(u8) = .empty;
                    var vs_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &vs_buf);
                    zigts.property_diagnostics.formatViolationsSummary(&vs_aw.writer, all_violations.items, filename, null) catch {};
                    vs_buf = vs_aw.toArrayList();
                    if (vs_buf.items.len > 0) {
                        viol_summary_result = try vs_buf.toOwnedSlice(allocator);
                    } else {
                        vs_buf.deinit(allocator);
                    }
                }

                if (!builtin.is_test) std.debug.print("\nVerification failed for {s}\n", .{filename});
                return .{
                    .verify_failed = true,
                    .violations_jsonl = viol_jsonl_result,
                    .violations_summary = viol_summary_result,
                };
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
            state_isolated = !verifier.has_module_mutation;

            // unchecked_result_value and unchecked_optional_* are .err severity,
            // so reaching here (error_count == 0) guarantees both properties hold.
            result_safe = true;
            optional_safe = true;

            if (!builtin.is_test) std.debug.print("Verification passed\n", .{});
        } else {
            if (!builtin.is_test) std.debug.print("Warning: no handler function found for verification\n", .{});
        }
    }

    var code_gen = zigts.parser.CodeGen.initWithIRStore(
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

    if (!builtin.is_test) std.debug.print("Parsed successfully: {d} bytes of bytecode\n", .{func.code.len});

    // Bytecode verification: reject malformed bytecode before serialization
    const verify_bc = zigts.BytecodeVerifier.verify(&func);
    if (!verify_bc.valid) {
        std.debug.print("Bytecode verification failed at offset {d}: {s}\n", .{
            verify_bc.offset,
            verify_bc.message,
        });
        return error.BytecodeVerificationFailed;
    }

    // Get object literal shapes from parser
    const shapes = code_gen.shapes.items;
    if (!builtin.is_test) std.debug.print("Collected {d} object literal shapes\n", .{shapes.len});

    // Serialize bytecode with atoms AND shapes for complete cache format
    var buffer: [256 * 1024]u8 = undefined; // 256KB buffer
    var writer = zigts.bytecode_cache.SliceWriter{ .buffer = &buffer };

    zigts.bytecode_cache.serializeBytecodeWithAtomsAndShapes(&func, &atoms, shapes, &writer, allocator) catch |err| {
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
            // This is an early-return path (transpiler fallback); violations_out
            // is null here because PathGenerator hasn't run yet.
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
                    null,
                    stc_ptr,
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
            &all_violations,
            stc_ptr,
        );

        // Inject verification-derived properties (Checks 2, 6, 7)
        if (contract.?.properties) |*props| {
            props.state_isolated = state_isolated;
            props.result_safe = result_safe;
            props.optional_safe = optional_safe;
        }
    }

    // Generate exhaustive test cases from path analysis.
    // Also runs when emitting a contract, to populate behavioral paths.
    var generated_tests_jsonl: ?[]const u8 = null;
    var violations_jsonl: ?[]const u8 = null;
    var violations_summary: ?[]const u8 = null;
    if (generate_tests or (emit_contract and contract != null)) {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = findHandlerFunction(ir_view, root);
        if (handler_fn) |hf| {
            var gen = zigts.PathGenerator.init(allocator, ir_view, &atoms);
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

                // Populate behavioral contract from exhaustive paths
                contract.?.behaviors = try gen.toBehaviorPaths(allocator);
                contract.?.behaviors_exhaustive = gen.getTests().len < zigts.PathGenerator.MAX_PATHS;
            }

            // Fault coverage analysis on generated paths
            var fc = zigts.fault_coverage.FaultCoverageChecker.init(allocator, gen.getTests());
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

            // Collect fault coverage violations into the unified all_violations list.
            // Flow and verifier violations were already collected earlier.
            if (fc_report.warning_count > 0) {
                zigts.property_diagnostics.collectFaultViolations(
                    allocator,
                    &all_violations,
                    fc_report.diagnostics,
                    gen.getTests(),
                );
            }

            // Fill counterexample refs for result_unsafe violations now that tests exist.
            zigts.property_diagnostics.fillVerifierCounterexamples(
                all_violations.items,
                gen.getTests(),
            );

            // Generate JSONL counterexamples and summary from all violations
            // (fault coverage, flow, and verifier combined).
            if (all_violations.items.len > 0) {
                var viol_buf: std.ArrayList(u8) = .empty;
                var viol_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &viol_buf);
                zigts.property_diagnostics.writeViolationsJsonl(
                    &viol_aw.writer,
                    allocator,
                    all_violations.items,
                    gen.getTests(),
                ) catch {};
                viol_buf = viol_aw.toArrayList();
                if (viol_buf.items.len > 0) {
                    violations_jsonl = try viol_buf.toOwnedSlice(allocator);
                } else {
                    viol_buf.deinit(allocator);
                }

                // Pre-format violations summary for build output. Path is unknown here;
                // the emit section in main() appends the file path separately.
                var sum_buf: std.ArrayList(u8) = .empty;
                var sum_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &sum_buf);
                zigts.property_diagnostics.formatViolationsSummary(
                    &sum_aw.writer,
                    all_violations.items,
                    filename,
                    null,
                ) catch {};
                sum_buf = sum_aw.toArrayList();
                if (sum_buf.items.len > 0) {
                    violations_summary = try sum_buf.toOwnedSlice(allocator);
                } else {
                    sum_buf.deinit(allocator);
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
        .violations_jsonl = violations_jsonl,
        .violations_summary = violations_summary,
    };
}

fn resolveImportedAtomName(
    atom_value: u32,
    atoms: ?*zigts.context.AtomTable,
) ?[]const u8 {
    const atom: zigts.object.Atom = @enumFromInt(atom_value);
    if (atom.isPredefined()) return atom.toPredefinedName();
    if (atoms) |table| return table.getName(atom);
    return null;
}

fn validateVirtualModuleImports(
    view: ir.IrView,
    atoms: ?*zigts.context.AtomTable,
    filename: []const u8,
) !void {
    const node_count = view.nodeCount();
    for (0..node_count) |idx| {
        const node_idx: ir.NodeIndex = @intCast(idx);
        const tag = view.getTag(node_idx) orelse continue;
        if (tag != .import_decl) continue;

        const import_decl = view.getImportDecl(node_idx) orelse continue;
        const module_str = view.getString(import_decl.module_idx) orelse continue;
        const binding = zigts.builtin_modules.fromSpecifier(module_str) orelse continue;

        var name_buf: [32][]const u8 = undefined;
        var name_count: usize = 0;
        var j: u8 = 0;
        while (j < import_decl.specifiers_count) : (j += 1) {
            const spec_idx = view.getListIndex(import_decl.specifiers_start, j);
            const spec = view.getImportSpec(spec_idx) orelse continue;
            const imported_name = resolveImportedAtomName(spec.imported_atom, atoms) orelse continue;
            if (name_count < name_buf.len) {
                name_buf[name_count] = imported_name;
                name_count += 1;
            }
        }

        if (zigts.modules.validateImports(binding, name_buf[0..name_count])) |missing| {
            if (!builtin.is_test) std.debug.print(
                "import error: module '{s}' does not export '{s}'\n  --> {s}\n",
                .{ module_str, missing, filename },
            );
            return error.InvalidImportSpecifier;
        }
    }
}

/// Scan parsed IR for file import declarations
fn hasFileImports(js_parser: *zigts.parser.JsParser, _: ir.NodeIndex) bool {
    const view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const node_count = view.nodeCount();

    for (0..node_count) |idx| {
        const tag = view.getTag(@intCast(idx)) orelse continue;
        if (tag != .import_decl) continue;

        const import_decl = view.getImportDecl(@intCast(idx)) orelse continue;
        const module_str = view.getString(import_decl.module_idx) orelse continue;

        const result = zigts.modules.resolver.resolve(module_str);
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
    strings: *zigts.string.StringTable,
    atoms: *zigts.context.AtomTable,
    needs_contract: bool,
    emit_contract: bool,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
    service_type_context: ?*const ServiceTypeContext,
) !CompiledHandler {
    // Build module graph
    var graph = zigts.modules.ModuleGraph.init(allocator);
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
    var module_compiler = zigts.modules.ModuleCompiler.init(allocator, atoms, strings);
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
        var writer = zigts.bytecode_cache.SliceWriter{ .buffer = &buffer };

        zigts.bytecode_cache.serializeBytecodeWithAtomsAndShapes(
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
    var entry_writer = zigts.bytecode_cache.SliceWriter{ .buffer = &entry_buffer };

    zigts.bytecode_cache.serializeBytecodeWithAtomsAndShapes(
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
            service_type_context,
        );
    }

    return .{
        .bytecode = entry_bytecode,
        .dep_bytecodes = dep_bytecodes,
        .contract = contract,
    };
}

const readFilePosixForGraph = zigts.file_io.readFileForModuleGraph;

fn analyzeAot(
    allocator: std.mem.Allocator,
    js_parser: *zigts.parser.JsParser,
    atoms: *zigts.context.AtomTable,
    root: ir.NodeIndex,
) !?AotAnalysis {
    const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
    const handler_fn = findHandlerFunction(ir_view, root) orelse return null;

    var analyzer = zigts.HandlerAnalyzer.init(allocator, ir_view, atoms);
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

    var handler_loc: ?zigts.parser.SourceLocation = null;
    if (handler_fn < js_parser.nodes.locs.items.len) {
        handler_loc = js_parser.nodes.locs.items[handler_fn];
    }

    return .{
        .dispatch = dispatch,
        .default_response = default_response,
        .handler_loc = handler_loc,
    };
}

fn countAotPatterns(dispatch: *const zigts.PatternDispatchTable) usize {
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

// findHandlerFunction is defined in handler_verifier.zig and exported via zigts.
const findHandlerFunction = zigts.handler_verifier.findHandlerFunction;

/// Build a contract manifest from the parsed IR.
fn buildContract(
    allocator: std.mem.Allocator,
    js_parser: *zigts.parser.JsParser,
    atoms: *zigts.context.AtomTable,
    filename: []const u8,
    root: ir.NodeIndex,
    aot: ?AotAnalysis,
    verify_info: ?VerificationInfo,
    type_map: ?*const zigts.TypeMap,
    service_type_context: ?*const ServiceTypeContext,
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

    var type_pool = zigts.TypePool.init(allocator);
    defer type_pool.deinit(allocator);

    var type_env = zigts.TypeEnv.init(allocator, &type_pool);
    defer type_env.deinit();
    zigts.modules.populateModuleTypes(&type_env, &type_pool, allocator);
    if (type_map) |tm| {
        type_env.populateFromTypeMap(tm);
    }

    var type_checker = zigts.TypeChecker.init(allocator, ir_view, atoms, &type_env, service_type_context);
    defer type_checker.deinit();
    _ = type_checker.check(root) catch 0;

    var builder = ContractBuilder.init(allocator, ir_view, atoms, &type_env, &type_checker);
    defer builder.deinit();

    const dispatch = if (aot) |a| a.dispatch else null;
    const has_default = if (aot) |a| a.default_response != null else false;

    return builder.build(
        filename,
        handler_loc,
        handler_fn,
        dispatch,
        has_default,
        verify_info,
    );
}

fn buildContractWithPolicy(
    allocator: std.mem.Allocator,
    js_parser: *zigts.parser.JsParser,
    atoms: *zigts.context.AtomTable,
    filename: []const u8,
    root: ir.NodeIndex,
    aot: ?AotAnalysis,
    verify_info: ?VerificationInfo,
    type_map: ?*const zigts.TypeMap,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
    violations_out: ?*std.ArrayList(zigts.property_diagnostics.PropertyViolation),
    service_type_context: ?*const ServiceTypeContext,
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
        service_type_context,
    );
    errdefer contract.deinit(allocator);

    if (contract.scope.used and contract.durable.used) {
        return error.ScopeDurableUnsupported;
    }

    // Run data flow provenance analysis
    {
        const ir_view = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        const handler_fn = findHandlerFunction(ir_view, root);
        if (handler_fn) |hf| {
            var flow = zigts.FlowChecker.init(allocator, ir_view, atoms);
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
                props.injection_safe = flow_props.injection_safe;
            }

            // Collect flow violations while flow is still alive (strings are duped).
            if (violations_out) |vout| {
                const ir_view_flow = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
                zigts.property_diagnostics.collectFlowViolations(allocator, vout, flow_diags, ir_view_flow);
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
    graph: *const zigts.modules.ModuleGraph,
    compile_result: *const zigts.modules.CompileResult,
    atoms: *zigts.context.AtomTable,
    entry_filename: []const u8,
    emit_contract: bool,
    policy: ?HandlerPolicy,
    sql_schema_path: ?[]const u8,
    service_type_context: ?*const ServiceTypeContext,
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
            service_type_context,
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
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
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

    target.scope.used = target.scope.used or source.scope.used;
    for (source.scope.names.items) |name| {
        try appendUniqueString(allocator, &target.scope.names, name, false);
    }
    target.scope.dynamic = target.scope.dynamic or source.scope.dynamic;
    target.scope.max_depth = @max(target.scope.max_depth, source.scope.max_depth);

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
            .query_params_dynamic = route.query_params_dynamic,
            .header_params_dynamic = route.header_params_dynamic,
            .request_bodies_dynamic = route.request_bodies_dynamic,
            .responses_dynamic = route.responses_dynamic,
            .response_status = route.response_status,
            .response_content_type = if (route.response_content_type) |content_type|
                try allocator.dupe(u8, content_type)
            else
                null,
            .response_schema_ref = if (route.response_schema_ref) |schema_ref|
                try allocator.dupe(u8, schema_ref)
            else
                null,
            .response_schema_json = if (route.response_schema_json) |schema_json|
                try allocator.dupe(u8, schema_json)
            else
                null,
            .response_schema_dynamic = route.response_schema_dynamic,
        };
        errdefer route_copy.deinit(allocator);

        for (route.request_schema_refs.items) |schema_ref| {
            try appendUniqueString(allocator, &route_copy.request_schema_refs, schema_ref, false);
        }
        for (route.path_params.items) |param| {
            try route_copy.path_params.append(allocator, try param.dupeOwned(allocator));
        }
        for (route.query_params.items) |param| {
            try route_copy.query_params.append(allocator, try param.dupeOwned(allocator));
        }
        for (route.header_params.items) |param| {
            try route_copy.header_params.append(allocator, try param.dupeOwned(allocator));
        }
        for (route.request_bodies.items) |body| {
            try route_copy.request_bodies.append(allocator, try body.dupeOwned(allocator));
        }
        for (route.responses.items) |response| {
            try route_copy.responses.append(allocator, try response.dupeOwned(allocator));
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
        .{ .name = "injection_safe", .value = props.injection_safe, .desc = "no unvalidated input in sinks" },
        .{ .name = "idempotent", .value = props.idempotent, .desc = "safe for at-least-once delivery" },
        .{ .name = "state_isolated", .value = props.state_isolated, .desc = "no cross-request data leakage" },
        .{ .name = "result_safe", .value = props.result_safe, .desc = "all result.ok accesses guarded (requires -Dverify)" },
        .{ .name = "optional_safe", .value = props.optional_safe, .desc = "all optionals narrowed before use (requires -Dverify)" },
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

fn writeSdkArtifact(
    allocator: std.mem.Allocator,
    output_path: []const u8,
    contract: *const HandlerContract,
    sdk_target: []const u8,
) !void {
    if (!std.mem.eql(u8, sdk_target, "ts")) {
        std.debug.print("Unknown SDK target: {s} (supported: ts)\n", .{sdk_target});
        return error.InvalidArgument;
    }

    const sdk_path = deriveSiblingPath(allocator, output_path, "client.ts") catch |err| {
        std.debug.print("Error deriving SDK path: {}\n", .{err});
        return err;
    };
    defer allocator.free(sdk_path);

    var sdk_output: std.ArrayList(u8) = .empty;
    defer sdk_output.deinit(allocator);
    var sdk_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &sdk_output);

    sdk_codegen.writeTypeScriptClient(&sdk_aw.writer, allocator, contract, .{}) catch |err| {
        std.debug.print("Error serializing SDK artifact: {}\n", .{err});
        return err;
    };
    sdk_output = sdk_aw.toArrayList();

    writeFilePosix(sdk_path, sdk_output.items, allocator) catch |err| {
        std.debug.print("Error writing SDK file '{s}': {}\n", .{ sdk_path, err });
        return err;
    };

    if (!builtin.is_test) std.debug.print("Wrote TypeScript SDK to: {s}\n", .{sdk_path});
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
    try writer.writeAll("const zq = @import(\"zigts\");\n\n");

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
    try writer.writeAll("\npub const capability_policy = @import(\"zigts\").handler_policy.RuntimePolicy{\n");
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
        null,
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
            null,
        ),
    );
}

fn buildTestContractForSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    sql_schema_path: ?[]const u8,
) !HandlerContract {
    var strings = zigts.StringTable.init(allocator);
    defer strings.deinit();

    var atoms = zigts.context.AtomTable.init(allocator);
    defer atoms.deinit();

    var js_parser = zigts.parser.JsParser.init(allocator, source);
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
        null,
        null,
    );
}

test "buildTestContractForSource keeps decodeQuery schemas out of request bodies" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const source =
        \\import { routerMatch } from "zigttp:router";
        \\import { schemaCompile } from "zigttp:validate";
        \\import { decodeQuery } from "zigttp:decode";
        \\
        \\schemaCompile("search.query", JSON.stringify({
        \\  type: "object",
        \\  properties: {
        \\    verbose: { type: "boolean" },
        \\  },
        \\}));
        \\
        \\function search(req) {
        \\  const query = decodeQuery("search.query", req.query ?? {});
        \\  return Response.json(true);
        \\}
        \\
        \\const routes = {
        \\  "GET /search": search,
        \\};
        \\
        \\export function handler(req) {
        \\  const found = routerMatch(routes, req);
        \\  if (found !== undefined) return found.handler(req);
        \\  return Response.json({ error: "not found" }, { status: 404 });
        \\}
    ;

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "handler.js", .data = source });

    const allocator = std.testing.allocator;
    const entry_path = try tmp.dir.realPathFileAlloc(std.testing.io, "handler.js", allocator);
    defer allocator.free(entry_path);

    var contract = try buildTestContractForSource(allocator, source, entry_path, null);
    defer contract.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), contract.api.routes.items.len);
    const route = contract.api.routes.items[0];
    try std.testing.expectEqualStrings("GET", route.method);
    try std.testing.expectEqualStrings("/search", route.path);
    try std.testing.expectEqual(@as(usize, 1), route.query_params.items.len);
    try std.testing.expectEqualStrings("verbose", route.query_params.items[0].name);
    try std.testing.expectEqualStrings("{\"type\":\"boolean\"}", route.query_params.items[0].schema_json);
    try std.testing.expectEqual(@as(usize, 0), route.request_bodies.items.len);
    try std.testing.expectEqual(@as(usize, 0), route.request_schema_refs.items.len);
}

test "buildTestContractForSource extracts durable workflow contract" {
    const allocator = std.testing.allocator;
    const source =
        \\import { run, step, waitSignal } from "zigttp:durable";
        \\
        \\function handler(req) {
        \\  const isPost = req.method === "POST";
        \\  return run("job:123", () => {
        \\    const order = step("load", () => 1);
        \\    if (isPost) {
        \\      return Response.text(`loaded:${order}`, { status: 202 });
        \\    }
        \\    const payload = waitSignal("approved");
        \\    return Response.json(payload);
        \\  });
        \\}
    ;

    var contract = try buildTestContractForSource(allocator, source, "durable-workflow.ts", null);
    defer contract.deinit(allocator);

    try std.testing.expect(contract.durable.used);
    try std.testing.expectEqual(handler_contract.DurableWorkflowProofLevel.complete, contract.durable.workflow.proof_level);
    try std.testing.expect(contract.durable.workflow.workflow_id != null);
    try std.testing.expect(std.mem.startsWith(u8, contract.durable.workflow.workflow_id.?, "durable-workflow.ts:handler:"));

    var saw_branch = false;
    var saw_step = false;
    var saw_wait_signal = false;
    var saw_return = false;
    for (contract.durable.workflow.nodes.items) |node| {
        switch (node.kind) {
            .branch => saw_branch = true,
            .step => {
                saw_step = true;
                try std.testing.expectEqualStrings("load", node.label);
            },
            .wait_signal => {
                saw_wait_signal = true;
                try std.testing.expectEqualStrings("approved", node.label);
            },
            .return_response => saw_return = true,
            else => {},
        }
    }
    try std.testing.expect(saw_branch);
    try std.testing.expect(saw_step);
    try std.testing.expect(saw_wait_signal);
    try std.testing.expect(saw_return);
    try std.testing.expect(contract.durable.workflow.edges.items.len >= 3);
}

test "buildTestContractForSource marks durable workflow partial for dynamic signal name" {
    const allocator = std.testing.allocator;
    const source =
        \\import { run, waitSignal } from "zigttp:durable";
        \\
        \\function handler(req) {
        \\  return run("job:123", () => {
        \\    const signalName = req.method;
        \\    const payload = waitSignal(signalName);
        \\    return Response.json(payload);
        \\  });
        \\}
    ;

    var contract = try buildTestContractForSource(allocator, source, "durable-partial.ts", null);
    defer contract.deinit(allocator);

    try std.testing.expect(contract.durable.used);
    try std.testing.expect(contract.durable.signals.dynamic);
    try std.testing.expectEqual(handler_contract.DurableWorkflowProofLevel.partial, contract.durable.workflow.proof_level);

    var saw_dynamic_signal = false;
    for (contract.durable.workflow.nodes.items) |node| {
        if (node.kind != .wait_signal) continue;
        saw_dynamic_signal = true;
        try std.testing.expectEqualStrings("<dynamic signal>", node.label);
    }
    try std.testing.expect(saw_dynamic_signal);
}

test "buildTestContractForSource extracts scope metadata and clears retry safety" {
    const allocator = std.testing.allocator;
    const source =
        \\import { scope, ensure } from "zigttp:scope";
        \\
        \\function handler(req) {
        \\  return scope("outer", () => {
        \\    ensure(() => {});
        \\    return scope("inner", () => {
        \\      return Response.json({ ok: true });
        \\    });
        \\  });
        \\}
    ;

    var contract = try buildTestContractForSource(allocator, source, "scope.ts", null);
    defer contract.deinit(allocator);

    try std.testing.expect(contract.scope.used);
    try std.testing.expectEqual(@as(u32, 2), contract.scope.max_depth);
    try std.testing.expect(!contract.properties.?.retry_safe);
    try std.testing.expect(handler_contract.containsString(contract.scope.names.items, "outer"));
    try std.testing.expect(handler_contract.containsString(contract.scope.names.items, "inner"));
}

test "buildTestContractForSource rejects scope and durable together" {
    const allocator = std.testing.allocator;
    const source =
        \\import { scope } from "zigttp:scope";
        \\import { run } from "zigttp:durable";
        \\
        \\function handler(req) {
        \\  return run("job:123", () => {
        \\    return scope("inner", () => Response.json({ ok: true }));
        \\  });
        \\}
    ;

    try std.testing.expectError(
        error.ScopeDurableUnsupported,
        buildTestContractForSource(allocator, source, "scope-durable.ts", null),
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

test "compileHandler rejects invalid virtual-module imports" {
    const allocator = std.testing.allocator;
    const source =
        \\import { definitelyMissing } from "zigttp:sql";
        \\
        \\function handler(req) {
        \\  return Response.json({ ok: definitelyMissing });
        \\}
    ;

    try std.testing.expectError(
        error.InvalidImportSpecifier,
        compileHandler(
            allocator,
            source,
            "handler.js",
            false,
            false,
            false,
            null,
            null,
            false,
            null,
        ),
    );
}

test "resolveGeneratorPack parses integration paths" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const pack_json =
        \\{
        \\  "sqlSchema": "schema.sql",
        \\  "manifest": "governance-manifest.json",
        \\  "expectProperties": "handler-properties.expected.json",
        \\  "dataLabels": "data-labels.json",
        \\  "replay": "simulation-traces.jsonl",
        \\  "faultSeverity": "fault-severity.json",
        \\  "report": "json"
        \\}
    ;

    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "pack.json", .data = pack_json });

    const allocator = std.testing.allocator;
    const pack_path = try tmp.dir.realPathFileAlloc(std.testing.io, "pack.json", allocator);
    defer allocator.free(pack_path);

    var pack = try resolveGeneratorPack(allocator, pack_path);
    defer pack.deinit(allocator);

    try std.testing.expect(std.mem.endsWith(u8, pack.sql_schema_path.?, "/schema.sql"));
    try std.testing.expect(std.mem.endsWith(u8, pack.manifest_path.?, "/governance-manifest.json"));
    try std.testing.expectEqualStrings("json", pack.report_format.?);
}

test "writeSdkArtifact writes client sibling file" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const allocator = std.testing.allocator;
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "embedded_handler.zig", .data = "" });
    const output_path = try tmp.dir.realPathFileAlloc(std.testing.io, "embedded_handler.zig", allocator);
    defer allocator.free(output_path);

    var schemas: std.ArrayList(handler_contract.ApiSchemaInfo) = .empty;
    try schemas.append(allocator, .{
        .name = try allocator.dupe(u8, "health"),
        .schema_json = try allocator.dupe(u8, "{\"type\":\"object\",\"properties\":{\"ok\":{\"type\":\"boolean\"}},\"required\":[\"ok\"]}"),
    });

    var routes: std.ArrayList(handler_contract.ApiRouteInfo) = .empty;
    try routes.append(allocator, .{
        .method = try allocator.dupe(u8, "GET"),
        .path = try allocator.dupe(u8, "/health"),
        .request_schema_refs = .empty,
        .request_schema_dynamic = false,
        .requires_bearer = false,
        .requires_jwt = false,
        .response_status = 200,
        .response_content_type = try allocator.dupe(u8, "application/json"),
        .response_schema_ref = try allocator.dupe(u8, "health"),
        .response_schema_dynamic = false,
    });

    var contract = HandlerContract{
        .handler = .{ .path = try allocator.dupe(u8, "health.ts"), .line = 1, .column = 1 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = handler_contract.emptySqlInfo(),
        .durable = .{
            .used = false,
            .keys = .{ .literal = .empty, .dynamic = false },
            .steps = .empty,
        },
        .scope = .{
            .used = false,
            .names = .empty,
            .dynamic = false,
            .max_depth = 0,
        },
        .api = .{
            .schemas = schemas,
            .requests = .{ .schema_refs = .empty, .dynamic = false },
            .auth = .{ .bearer = false, .jwt = false },
            .routes = routes,
            .schemas_dynamic = false,
            .routes_dynamic = false,
        },
        .verification = null,
        .aot = null,
    };
    defer contract.deinit(allocator);

    try writeSdkArtifact(allocator, output_path, &contract, "ts");

    const client_path = try tmp.dir.realPathFileAlloc(std.testing.io, "client.ts", allocator);
    defer allocator.free(client_path);
    const bytes = try readFilePosix(allocator, client_path, 64 * 1024);
    defer allocator.free(bytes);

    try std.testing.expect(std.mem.indexOf(u8, bytes, "async getHealth(") != null);
    try std.testing.expect(std.mem.indexOf(u8, bytes, "export type Health = {") != null);
}

test "compileHandler emits result_unsafe counterexample when jwtVerify result is unchecked" {
    const allocator = std.testing.allocator;
    // Handler accesses .value without checking .ok - triggers result_unsafe verification error
    const source =
        \\import { parseBearer } from "zigttp:auth";
        \\import { jwtVerify } from "zigttp:auth";
        \\
        \\function handler(req: Request): Response {
        \\  const token = parseBearer(req);
        \\  if (!token) return Response.json({ error: "no token" }, { status: 401 });
        \\  const result = jwtVerify(token, "secret");
        \\  return Response.json({ user: result.value });
        \\}
    ;

    var compiled = try compileHandler(
        allocator,
        source,
        "handler.ts",
        false, // emit_aot
        true, // emit_verify
        false, // emit_contract
        null, // policy
        null, // sql_schema_path
        false, // generate_tests
        null, // system_path
    );
    defer compiled.deinit(allocator);

    try std.testing.expect(compiled.verify_failed);
    // violations_jsonl must be populated with a result-unsafe counterexample
    const vj = compiled.violations_jsonl orelse return error.MissingViolationsJsonl;
    try std.testing.expect(std.mem.indexOf(u8, vj, "result-unsafe") != null);
    // The counterexample io stub must include a {ok:false} failure case for jwtVerify
    try std.testing.expect(std.mem.indexOf(u8, vj, "jwtVerify") != null);
    try std.testing.expect(std.mem.indexOf(u8, vj, "\"ok\":false") != null);
}

test "compileHandler sets result_safe and optional_safe when verification passes" {
    const allocator = std.testing.allocator;
    // Handler that properly checks jwtVerify result and env optional before use.
    // Uses Response.text to avoid object-literal AOT serialization (pre-existing
    // leak in serializeObjectLiteral is unrelated to this test).
    const source =
        \\import { parseBearer } from "zigttp:auth";
        \\import { jwtVerify } from "zigttp:auth";
        \\import { env } from "zigttp:env";
        \\
        \\function handler(req: Request): Response {
        \\  const token = parseBearer(req);
        \\  if (!token) return Response.text("no token", { status: 401 });
        \\  const result = jwtVerify(token, "secret");
        \\  if (!result.ok) return Response.text(result.error, { status: 401 });
        \\  const name = env("NAME") ?? "world";
        \\  return Response.text(name);
        \\}
    ;

    var compiled = try compileHandler(
        allocator,
        source,
        "handler.ts",
        false, // emit_aot
        true, // emit_verify
        true, // emit_contract
        null, // policy
        null, // sql_schema_path
        false, // generate_tests
        null, // system_path
    );
    defer compiled.deinit(allocator);

    try std.testing.expect(!compiled.verify_failed);
    const contract = compiled.contract orelse return error.MissingContract;
    const props = contract.properties orelse return error.MissingProperties;
    try std.testing.expect(props.result_safe);
    try std.testing.expect(props.optional_safe);
}
