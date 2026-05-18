//! Prove pipeline: contract upgrade verification, manifest alignment,
//! property expectations, and the build-report writer.

const std = @import("std");
const zigts = @import("zigts");
const util = @import("precompile_util.zig");
const buildtime = @import("precompile_buildtime.zig");
const manifest_alignment = @import("manifest_alignment.zig");
const property_expectations = @import("property_expectations.zig");
const build_report = @import("report.zig");
const prove_upgrade = @import("prove_upgrade.zig");

const handler_contract = zigts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;
const readFilePosix = zigts.file_io.readFile;

pub fn runProvePipeline(
    allocator: std.mem.Allocator,
    spec: []const u8,
    contract: *HandlerContract,
    source: []const u8,
    handler_path: []const u8,
    output_path: []const u8,
) !void {
    // Parse spec format: "contract.json" or "contract.json:traces.jsonl"
    var old_contract_path: []const u8 = spec;
    var prove_trace_path: ?[]const u8 = null;
    if (std.mem.indexOf(u8, spec, ":")) |colon_pos| {
        old_contract_path = spec[0..colon_pos];
        if (colon_pos + 1 < spec.len) {
            prove_trace_path = spec[colon_pos + 1 ..];
        }
    }

    const old_contract_json = readFilePosix(allocator, old_contract_path, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Error reading old contract '{s}': {}\n", .{ old_contract_path, err });
        return err;
    };
    defer allocator.free(old_contract_json);

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
            const replay_result = buildtime.runBuildTimeReplay(allocator, source, handler_path, groups) catch |err| {
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

    var result = prove_upgrade.prove(
        allocator,
        old_contract_json,
        contract,
        prove_replay,
        handler_path,
    ) catch |err| {
        std.debug.print("Proven evolution failed: {}\n", .{err});
        return err;
    };
    defer result.deinit(allocator);

    const prove_dir = util.deriveSiblingPath(allocator, output_path, "") catch |err| {
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

pub fn runManifestAlignment(
    allocator: std.mem.Allocator,
    manifest_path: []const u8,
    contract: *HandlerContract,
) !manifest_alignment.ManifestAlignment {
    const manifest_bytes = readFilePosix(allocator, manifest_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading manifest file '{s}': {}\n", .{ manifest_path, err });
        return err;
    };
    defer allocator.free(manifest_bytes);

    var parsed_manifest = manifest_alignment.parseManifest(allocator, manifest_bytes) catch |err| {
        std.debug.print("Error parsing manifest '{s}': {}\n", .{ manifest_path, err });
        return err;
    };
    defer parsed_manifest.deinit(allocator);

    var result = manifest_alignment.checkAlignment(allocator, &parsed_manifest, contract) catch |err| {
        std.debug.print("Error checking manifest alignment: {}\n", .{err});
        return err;
    };
    manifest_alignment.printAlignmentSummary(&result);
    return result;
}

pub fn runPropertyExpectations(
    allocator: std.mem.Allocator,
    expect_properties_path: []const u8,
    contract: *HandlerContract,
) !property_expectations.ExpectationResult {
    var result = property_expectations.checkExpectationsFromFile(allocator, expect_properties_path, contract) catch |err| {
        std.debug.print("Error checking property expectations: {}\n", .{err});
        return err;
    };
    property_expectations.printExpectationResults(&result);
    return result;
}

pub fn writeBuildReport(
    allocator: std.mem.Allocator,
    fmt: []const u8,
    contract: *HandlerContract,
    manifest_result: ?*manifest_alignment.ManifestAlignment,
    property_result: ?*property_expectations.ExpectationResult,
    integration_inputs: *const build_report.IntegrationSection,
    handler_path: []const u8,
    output_path: []const u8,
) !void {
    if (!std.mem.eql(u8, fmt, "json")) {
        std.debug.print("Unknown report format: {s} (supported: json)\n", .{fmt});
        return;
    }

    var handler_report = build_report.buildReport(
        allocator,
        contract,
        manifest_result,
        property_result,
        integration_inputs,
        handler_path,
    );
    defer {
        if (handler_report.verification) |v| allocator.free(v.checks);
        if (handler_report.manifest_alignment) |ma| allocator.free(ma.sections);
    }

    const report_path = util.deriveSiblingPath(allocator, output_path, "report.json") catch |err| {
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

    util.writeFilePosix(report_path, report_output.items, allocator) catch |err| {
        std.debug.print("Error writing report '{s}': {}\n", .{ report_path, err });
        return err;
    };

    std.debug.print("Wrote build report to: {s}\n", .{report_path});
}

