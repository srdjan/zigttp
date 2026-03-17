//! Build-time TypeScript/JavaScript precompiler
//!
//! Compiles handler files to bytecode at build time for embedding in the binary.
//! This eliminates runtime parsing overhead and removes the need for source files
//! in deployment.
//!
//! Usage: precompile [--aot] [--policy policy.json] <handler.ts> <output.zig>

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
    var policy_path: ?[]const u8 = null;
    var emit_sound = false;
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
        if (std.mem.eql(u8, arg, "--policy")) {
            policy_path = args.next() orelse {
                std.debug.print("Missing path after --policy\n", .{});
                return error.MissingArgument;
            };
            continue;
        }
        if (std.mem.eql(u8, arg, "--sound")) {
            emit_sound = true;
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
        std.debug.print("Usage: precompile [--aot] [--verify] [--contract] [--policy policy.json] [--sound] <handler.ts> <output.zig>\n", .{});
        std.debug.print("\nCompiles a TypeScript/JavaScript handler to bytecode.\n", .{});
        return error.MissingArgument;
    };

    const output_path_final = output_path orelse {
        std.debug.print("Usage: precompile [--aot] [--verify] [--contract] [--policy policy.json] [--sound] <handler.ts> <output.zig>\n", .{});
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
        emit_sound,
    ) catch |err| {
        std.debug.print("Compilation failed: {}\n", .{err});
        return err;
    };
    defer compiled.deinit(allocator);

    std.debug.print("Generated bytecode: {d} bytes\n", .{compiled.bytecode.len});
    if (compiled.aot != null) {
        std.debug.print("AOT analysis enabled\n", .{});
    }

    // Write the output Zig file
    writeZigFile(output_path_final, compiled, handler_path_final, policy, allocator) catch |err| {
        std.debug.print("Error writing output file '{s}': {}\n", .{ output_path_final, err });
        return err;
    };

    std.debug.print("Wrote embedded handler to: {s}\n", .{output_path_final});

    // Write contract.json alongside the output if requested
    if (emit_contract) {
        if (compiled.contract) |*contract| {
            const contract_path = deriveContractPath(allocator, output_path_final) catch |err| {
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
    }
}

fn compileHandler(
    allocator: std.mem.Allocator,
    source: []const u8,
    filename: []const u8,
    emit_aot: bool,
    emit_verify: bool,
    emit_contract: bool,
    policy: ?HandlerPolicy,
    emit_sound: bool,
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

    const needs_contract = emit_contract or policy != null;

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

    // Run sound mode bool checker if requested
    if (emit_sound) {
        const ir_view_sound = ir.IrView.fromIRStore(&js_parser.nodes, &js_parser.constants);
        var checker = zts.BoolChecker.init(allocator, ir_view_sound, &atoms);
        defer checker.deinit();

        const sound_error_count = try checker.check(root);
        const sound_diags = checker.getDiagnostics();

        if (sound_diags.len > 0) {
            std.debug.print("\n", .{});
            var sound_output: std.ArrayList(u8) = .empty;
            defer sound_output.deinit(allocator);
            var sound_aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &sound_output);
            checker.formatDiagnostics(source_to_parse, &sound_aw.writer) catch {};
            sound_output = sound_aw.toArrayList();
            if (sound_output.items.len > 0) {
                std.debug.print("{s}", .{sound_output.items});
            }
            std.debug.print("{d} sound mode error(s), {d} warning(s)\n", .{
                sound_error_count,
                sound_diags.len - sound_error_count,
            });
        }

        if (sound_error_count > 0) {
            std.debug.print("\nSound mode check failed for {s}\n", .{filename});
            return error.SoundModeViolation;
        }

        std.debug.print("Sound mode check passed\n", .{});
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

/// Derive contract.json path from the output .zig path.
/// e.g. "src/generated/embedded_handler.zig" -> "src/generated/contract.json"
fn deriveContractPath(allocator: std.mem.Allocator, output_path: []const u8) ![]u8 {
    // Find the last '/' to get the directory
    var dir_end: usize = 0;
    for (output_path, 0..) |c, i| {
        if (c == '/') dir_end = i + 1;
    }

    const dir = output_path[0..dir_end];
    const result = try allocator.alloc(u8, dir.len + "contract.json".len);
    @memcpy(result[0..dir.len], dir);
    @memcpy(result[dir.len..], "contract.json");
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
        try writeCapabilityPolicy(writer, policy);

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

    try writeCapabilityPolicy(writer, policy);

    output = aw.toArrayList();
    try writeFilePosix(path, output.items, allocator);
}

fn writeCapabilityPolicy(writer: anytype, policy: ?HandlerPolicy) !void {
    try writer.writeAll("\npub const capability_policy = @import(\"zts\").handler_policy.RuntimePolicy{\n");
    if (policy) |p| {
        try writePolicySection(writer, "env", p.env);
        try writePolicySection(writer, "egress", p.egress);
        try writePolicySection(writer, "cache", p.cache);
    } else {
        try writePolicySection(writer, "env", null);
        try writePolicySection(writer, "egress", null);
        try writePolicySection(writer, "cache", null);
    }
    try writer.writeAll("};\n");
}

fn writePolicySection(writer: anytype, field_name: []const u8, section: ?handler_policy.AllowList) !void {
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
            false,
        ),
    );
}
