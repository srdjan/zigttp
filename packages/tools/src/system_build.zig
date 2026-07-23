//! System Build Tool
//!
//! Orchestrates cross-handler contract linking. Reads a system.json file,
//! compiles each handler to extract its contract, runs the system linker,
//! and writes system-contract.json and system-report.txt.
//!
//! Usage: zts link <system.json> [--output-dir <dir>]

const std = @import("std");
const zts = @import("zts");
const precompile = @import("precompile.zig");
const system_linker = zts.system_linker;
const handler_contract = zts.handler_contract;

/// Optional signed-receipt hook. The keyless tools layer cannot reach the
/// persistent attest identity, so the developer CLI injects a runtime signer
/// (`hypermedia_probe_lib.recordWorkflowReceipt`) here at startup. Null in the
/// standalone `zts` binary, which emits no receipt. Called after a successful
/// link with the bundle's output dir and analysis.
pub var receipt_probe: ?*const fn (
    std.mem.Allocator,
    []const u8,
    []const u8,
    *const system_linker.SystemAnalysis,
) void = null;

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var system_path: ?[]const u8 = null;
    var output_dir: []const u8 = "./";

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--output-dir")) {
            i += 1;
            if (i >= argv.len) {
                std.debug.print("Error: --output-dir requires a path argument\n", .{});
                std.process.exit(2);
            }
            output_dir = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
        if (!std.mem.startsWith(u8, arg, "-") and system_path == null) {
            system_path = arg;
            continue;
        }
        std.debug.print("Error: unknown argument '{s}'\n", .{arg});
        printHelp();
        std.process.exit(2);
    }

    const path = system_path orelse {
        std.debug.print("Error: system.json path required\n", .{});
        printHelp();
        std.process.exit(2);
    };

    try runLink(allocator, path, output_dir);
}

fn runLink(allocator: std.mem.Allocator, system_path: []const u8, output_dir: []const u8) !void {
    const system_json = zts.file_io.readFile(allocator, system_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ system_path, err });
        std.process.exit(2);
    };
    defer allocator.free(system_json);

    var config = system_linker.parseSystemConfig(allocator, system_json) catch |err| {
        std.debug.print("Error parsing {s}: {}\n", .{ system_path, err });
        std.process.exit(2);
    };
    try precompile.resolveSystemHandlerPaths(allocator, system_path, &config);

    if (config.handlers.len == 0) {
        std.debug.print("Error: system.json contains no handlers\n", .{});
        config.deinit(allocator);
        std.process.exit(2);
    }

    // A declared entry must name a real bundle member - a typo here would
    // otherwise sign a receipt attesting to a door that doesn't exist.
    if (config.entry) |entry_name| {
        if (system_linker.findHandlerByName(config, entry_name) == null) {
            std.debug.print("Error: entry \"{s}\" does not match any handler name in system.json\n", .{entry_name});
            config.deinit(allocator);
            std.process.exit(1);
        }
    }

    std.debug.print("System: {d} handlers\n", .{config.handlers.len});

    var contracts = try allocator.alloc(handler_contract.HandlerContract, config.handlers.len);
    // `alloc` returns uninitialized memory; walk only the populated prefix
    // on cleanup so a partial-fill failure does not deinit garbage. Same
    // idiom as `precompile.zig` after b8f0bbb.
    var contracts_initialized: usize = 0;
    defer {
        for (contracts[0..contracts_initialized]) |*c| c.deinit(allocator);
        allocator.free(contracts);
    }

    var total_errors: u32 = 0;

    for (config.handlers, 0..) |entry, idx| {
        std.debug.print("  [{d}/{d}] Checking {s}...", .{ idx + 1, config.handlers.len, entry.path });

        var result = precompile.runCheckOnly(allocator, entry.path, null, false, system_path) catch |err| {
            std.debug.print(" FAILED ({s})\n", .{@errorName(err)});
            total_errors += 1;
            // Initialize a minimal contract so we can continue
            const path_dupe = try allocator.dupe(u8, entry.path);
            contracts[idx] = handler_contract.emptyContract(path_dupe);
            contracts_initialized = idx + 1;
            continue;
        };

        const errors = result.totalErrors();
        if (errors > 0) {
            std.debug.print(" {d} errors\n", .{errors});
            total_errors += errors;
        } else {
            std.debug.print(" ok\n", .{});
        }

        if (result.contract) |c| {
            contracts[idx] = c;
            result.contract = null; // prevent double-free
        } else {
            const path_dupe = try allocator.dupe(u8, entry.path);
            contracts[idx] = handler_contract.emptyContract(path_dupe);
        }
        contracts_initialized = idx + 1;

        result.deinit(allocator);
    }

    if (total_errors > 0) {
        std.debug.print("\n{d} compilation errors across handlers\n", .{total_errors});
        config.deinit(allocator);
        std.process.exit(1);
    }

    std.debug.print("\nLinking system...\n", .{});
    var analysis = system_linker.linkSystem(allocator, contracts, config) catch |err| {
        std.debug.print("Error during system linking: {}\n", .{err});
        config.deinit(allocator);
        std.process.exit(2);
    };
    // config ownership transferred to analysis
    defer analysis.deinit(allocator);

    // Write system-contract.json
    {
        const json_path = try std.fs.path.join(allocator, &.{ output_dir, "system-contract.json" });
        defer allocator.free(json_path);

        var json_output: std.ArrayList(u8) = .empty;
        defer json_output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &json_output);

        system_linker.writeSystemContractJson(
            &analysis,
            &aw.writer,
        ) catch |err| {
            std.debug.print("Error serializing system contract: {}\n", .{err});
            std.process.exit(2);
        };
        json_output = aw.toArrayList();

        precompile.writeFilePosix(json_path, json_output.items, allocator) catch |err| {
            std.debug.print("Error writing {s}: {}\n", .{ json_path, err });
            std.process.exit(2);
        };

        std.debug.print("Wrote {s}\n", .{json_path});
    }

    // Write system-report.txt
    {
        const report_path = try std.fs.path.join(allocator, &.{ output_dir, "system-report.txt" });
        defer allocator.free(report_path);

        var report_output: std.ArrayList(u8) = .empty;
        defer report_output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &report_output);

        system_linker.writeSystemReport(
            &analysis,
            &aw.writer,
        ) catch |err| {
            std.debug.print("Error serializing system report: {}\n", .{err});
            std.process.exit(2);
        };
        report_output = aw.toArrayList();

        precompile.writeFilePosix(report_path, report_output.items, allocator) catch |err| {
            std.debug.print("Error writing {s}: {}\n", .{ report_path, err });
            std.process.exit(2);
        };

        std.debug.print("Wrote {s}\n", .{report_path});
    }

    // Print summary
    std.debug.print("\n--- Summary ---\n", .{});
    std.debug.print("Links: {d} resolved, {d} unresolved\n", .{
        analysis.links.items.len,
        analysis.unresolved.items.len,
    });
    if (analysis.affordance_links.items.len > 0 or analysis.dangling_affordances > 0 or analysis.dynamic_affordances > 0) {
        std.debug.print("Affordances: {d} resolved, {d} dangling, {d} dynamic\n", .{
            analysis.affordance_links.items.len,
            analysis.dangling_affordances,
            analysis.dynamic_affordances,
        });
    }
    std.debug.print("Proof level: {s}\n", .{@tagName(analysis.proof_level)});

    if (analysis.warnings.items.len > 0) {
        std.debug.print("\nWarnings:\n", .{});
        for (analysis.warnings.items) |w| {
            std.debug.print("  {s}\n", .{w});
        }
    }

    // A dangling hypermedia affordance is a hard miss: fail the build so the
    // dangling link cannot ship (mirrors the unlinked-route gate below).
    if (analysis.dangling_affordances > 0) {
        std.process.exit(1);
    }

    // Exit with error if there are unlinked routes
    for (analysis.unresolved.items) |u| {
        if (u.status == .unlinked) {
            std.process.exit(1);
        }
    }

    // Sign a kind=workflow receipt over the hypermedia verdict when a runtime
    // signer is injected (the developer `zttp` binary). Emitted only AFTER the
    // failure gates above, so a failed link never leaves a signed receipt on
    // disk to be mistaken for a passing bundle. Best-effort.
    if (receipt_probe) |probe| {
        probe(allocator, system_path, output_dir, &analysis);
    }
}

fn printHelp() void {
    std.debug.print(
        \\Usage: zts link <system.json> [options]
        \\
        \\Cross-handler contract linking. Proves that a system of handlers
        \\communicates correctly at compile time.
        \\
        \\Options:
        \\  --output-dir <dir>  Output directory (default: ./)
        \\  --help              Show this help
        \\
        \\Input: system.json with handler paths and base URLs:
        \\  {{
        \\    "version": 1,
        \\    "handlers": [
        \\      {{ "name": "gateway", "path": "gateway.ts", "baseUrl": "https://gateway.internal" }},
        \\      {{ "name": "users", "path": "users.ts", "baseUrl": "https://users.internal" }}
        \\    ]
        \\  }}
        \\
        \\Output:
        \\  system-contract.json  Machine-readable system contract
        \\  system-report.txt     Human-readable analysis report
        \\
    , .{});
}
