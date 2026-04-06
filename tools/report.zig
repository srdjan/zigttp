//! Structured Build Report
//!
//! Aggregates all verification results into a single structured JSON report.
//! Each optional section is only populated when the corresponding data is
//! available (contract, manifest alignment, property expectations).
//!
//! Architecture: buildReport (contract + optional results) -> BuildReport -> writeReportJson
//!
//! Usage: called by precompile.zig when --report json is passed.

const std = @import("std");
const zigts = @import("zigts");
const handler_contract = zigts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;
const HandlerProperties = handler_contract.HandlerProperties;
const FaultCoverageInfo = handler_contract.FaultCoverageInfo;
const contract_diff = zigts.contract_diff;
const ProofLevel = contract_diff.ProofLevel;
const manifest_alignment = @import("manifest_alignment.zig");
const ManifestAlignment = manifest_alignment.ManifestAlignment;
const AlignmentResult = manifest_alignment.AlignmentResult;
const property_expectations = @import("property_expectations.zig");
const ExpectationResult = property_expectations.ExpectationResult;

const writeJsonString = handler_contract.writeJsonString;

// -------------------------------------------------------------------------
// Report data types
// -------------------------------------------------------------------------

pub const BuildReport = struct {
    handler: []const u8,
    proof_level: []const u8, // "complete", "partial", "none"

    verification: ?VerificationSection = null,
    properties: ?PropertiesSection = null,
    fault_coverage: ?FaultCoverageSection = null,
    flow_analysis: ?FlowAnalysisSection = null,
    manifest_alignment: ?ManifestAlignmentSection = null,
    property_expectations: ?PropertyExpectationsSection = null,
    integration: ?IntegrationSection = null,
};

pub const VerificationSection = struct {
    passed: bool,
    checks: []const []const u8,
};

pub const PropertiesSection = struct {
    state_isolated: bool,
    injection_safe: bool,
    read_only: bool,
    pure: bool,
    stateless: bool,
    retry_safe: bool,
    deterministic: bool,
    idempotent: bool,
    fault_covered: bool,
    no_secret_leakage: bool,
    no_credential_leakage: bool,
    input_validated: bool,
    pii_contained: bool,
};

pub const FaultCoverageSection = struct {
    total_failable: u32,
    covered: u32,
    warnings: u32,
    clean: bool,
};

pub const FlowAnalysisSection = struct {
    no_secret_leakage: bool,
    no_credential_leakage: bool,
    input_validated: bool,
    pii_contained: bool,
    injection_safe: bool,
};

pub const ManifestAlignmentSection = struct {
    overall: []const u8, // "pass", "warn", "fail"
    sections: []const SectionResult,
};

pub const SectionResult = struct {
    name: []const u8,
    status: []const u8,
    declared: u32,
    matched: u32,
};

pub const PropertyExpectationsSection = struct {
    passed: bool,
    checked_routes: u32,
    mismatches: u32,
};

pub const IntegrationSection = struct {
    generator_pack: bool = false,
    sql_schema: bool = false,
    manifest: bool = false,
    property_expectations: bool = false,
    data_labels: bool = false,
    replay: bool = false,
    fault_severity: bool = false,
};

// -------------------------------------------------------------------------
// Report construction
// -------------------------------------------------------------------------

/// Construct a BuildReport from available data. Each optional section is
/// populated only when the corresponding input is non-null.
/// All slices in the returned struct point into the contract or alignment
/// data - the caller must keep those alive while using the report.
pub fn buildReport(
    allocator: std.mem.Allocator,
    contract: *const HandlerContract,
    alignment: ?*const ManifestAlignment,
    prop_result: ?*const ExpectationResult,
    integration_inputs: ?*const IntegrationSection,
    handler_path: []const u8,
) BuildReport {
    const proof_level = contract_diff.deriveProofLevel(contract);

    // Verification section
    const verification: ?VerificationSection = if (contract.verification) |v| blk: {
        const passed = v.exhaustive_returns and v.results_safe and v.bytecode_verified;
        // Build checks list from static strings (no allocation needed)
        const checks = buildChecks(allocator, &v) catch &[_][]const u8{};
        break :blk .{ .passed = passed, .checks = checks };
    } else null;

    // Properties section
    const properties: ?PropertiesSection = if (contract.properties) |p| .{
        .state_isolated = p.state_isolated,
        .injection_safe = p.injection_safe,
        .read_only = p.read_only,
        .pure = p.pure,
        .stateless = p.stateless,
        .retry_safe = p.retry_safe,
        .deterministic = p.deterministic,
        .idempotent = p.idempotent,
        .fault_covered = p.fault_covered,
        .no_secret_leakage = p.no_secret_leakage,
        .no_credential_leakage = p.no_credential_leakage,
        .input_validated = p.input_validated,
        .pii_contained = p.pii_contained,
    } else null;

    // Fault coverage section
    const fault_coverage: ?FaultCoverageSection = if (contract.fault_coverage) |fc| .{
        .total_failable = fc.total_failable,
        .covered = fc.covered,
        .warnings = fc.warnings,
        .clean = fc.isCovered() and fc.warnings == 0,
    } else null;

    // Flow analysis section (derived from properties)
    const flow_analysis: ?FlowAnalysisSection = if (contract.properties) |p| .{
        .no_secret_leakage = p.no_secret_leakage,
        .no_credential_leakage = p.no_credential_leakage,
        .input_validated = p.input_validated,
        .pii_contained = p.pii_contained,
        .injection_safe = p.injection_safe,
    } else null;

    // Manifest alignment section
    const manifest_alignment_section: ?ManifestAlignmentSection = if (alignment) |a|
        buildAlignmentSection(allocator, a) catch null
    else
        null;

    // Property expectations section
    const property_expectations_section: ?PropertyExpectationsSection = if (prop_result) |r| .{
        .passed = r.passed,
        .checked_routes = r.checked_routes,
        .mismatches = @intCast(r.mismatches.len),
    } else null;

    const integration_section: ?IntegrationSection = if (integration_inputs) |inputs| inputs.* else null;

    return .{
        .handler = handler_path,
        .proof_level = proof_level.toString(),
        .verification = verification,
        .properties = properties,
        .fault_coverage = fault_coverage,
        .flow_analysis = flow_analysis,
        .manifest_alignment = manifest_alignment_section,
        .property_expectations = property_expectations_section,
        .integration = integration_section,
    };
}

fn buildChecks(allocator: std.mem.Allocator, v: *const handler_contract.VerificationInfo) ![]const []const u8 {
    var checks: std.ArrayList([]const u8) = .empty;
    errdefer checks.deinit(allocator);

    if (v.exhaustive_returns) try checks.append(allocator, "exhaustiveReturns");
    if (v.results_safe) try checks.append(allocator, "resultsSafe");
    if (v.bytecode_verified) try checks.append(allocator, "bytecodeVerified");

    return try checks.toOwnedSlice(allocator);
}

fn buildAlignmentSection(
    allocator: std.mem.Allocator,
    alignment: *const ManifestAlignment,
) !ManifestAlignmentSection {
    var sections: std.ArrayList(SectionResult) = .empty;
    errdefer sections.deinit(allocator);

    for (alignment.results) |result| {
        try sections.append(allocator, .{
            .name = result.section,
            .status = result.status.toString(),
            .declared = result.declared_count,
            .matched = result.matched_count,
        });
    }

    return .{
        .overall = alignment.overall.toString(),
        .sections = try sections.toOwnedSlice(allocator),
    };
}

// -------------------------------------------------------------------------
// JSON serialization
// -------------------------------------------------------------------------

/// Serialize a BuildReport to JSON. Uses manual writer calls matching the
/// style of handler_contract.zig's writeContractJson. Null sections are
/// omitted from the output.
pub fn writeReportJson(writer: anytype, report: *const BuildReport) !void {
    try writer.writeAll("{\n");

    // handler
    try writer.writeAll("  \"handler\": ");
    try writeJsonString(writer, report.handler);
    try writer.writeAll(",\n");

    // proofLevel
    try writer.writeAll("  \"proofLevel\": ");
    try writeJsonString(writer, report.proof_level);

    // verification (optional)
    if (report.verification) |v| {
        try writer.writeAll(",\n");
        try writer.writeAll("  \"verification\": {\n");
        try writer.print("    \"passed\": {s},\n", .{if (v.passed) "true" else "false"});
        try writer.writeAll("    \"checks\": [");
        for (v.checks, 0..) |check, i| {
            if (i > 0) try writer.writeAll(", ");
            try writeJsonString(writer, check);
        }
        try writer.writeAll("]\n");
        try writer.writeAll("  }");
    }

    // properties (optional)
    if (report.properties) |p| {
        try writer.writeAll(",\n");
        try writer.writeAll("  \"properties\": {\n");
        try writer.print("    \"stateIsolated\": {s},\n", .{if (p.state_isolated) "true" else "false"});
        try writer.print("    \"injectionSafe\": {s},\n", .{if (p.injection_safe) "true" else "false"});
        try writer.print("    \"readOnly\": {s},\n", .{if (p.read_only) "true" else "false"});
        try writer.print("    \"pure\": {s},\n", .{if (p.pure) "true" else "false"});
        try writer.print("    \"stateless\": {s},\n", .{if (p.stateless) "true" else "false"});
        try writer.print("    \"retrySafe\": {s},\n", .{if (p.retry_safe) "true" else "false"});
        try writer.print("    \"deterministic\": {s},\n", .{if (p.deterministic) "true" else "false"});
        try writer.print("    \"idempotent\": {s},\n", .{if (p.idempotent) "true" else "false"});
        try writer.print("    \"faultCovered\": {s},\n", .{if (p.fault_covered) "true" else "false"});
        try writer.print("    \"noSecretLeakage\": {s},\n", .{if (p.no_secret_leakage) "true" else "false"});
        try writer.print("    \"noCredentialLeakage\": {s},\n", .{if (p.no_credential_leakage) "true" else "false"});
        try writer.print("    \"inputValidated\": {s},\n", .{if (p.input_validated) "true" else "false"});
        try writer.print("    \"piiContained\": {s}\n", .{if (p.pii_contained) "true" else "false"});
        try writer.writeAll("  }");
    }

    // faultCoverage (optional)
    if (report.fault_coverage) |fc| {
        try writer.writeAll(",\n");
        try writer.writeAll("  \"faultCoverage\": {\n");
        try writer.print("    \"totalFailable\": {d},\n", .{fc.total_failable});
        try writer.print("    \"covered\": {d},\n", .{fc.covered});
        try writer.print("    \"warnings\": {d},\n", .{fc.warnings});
        try writer.print("    \"clean\": {s}\n", .{if (fc.clean) "true" else "false"});
        try writer.writeAll("  }");
    }

    // flowAnalysis (optional)
    if (report.flow_analysis) |fa| {
        try writer.writeAll(",\n");
        try writer.writeAll("  \"flowAnalysis\": {\n");
        try writer.print("    \"noSecretLeakage\": {s},\n", .{if (fa.no_secret_leakage) "true" else "false"});
        try writer.print("    \"noCredentialLeakage\": {s},\n", .{if (fa.no_credential_leakage) "true" else "false"});
        try writer.print("    \"inputValidated\": {s},\n", .{if (fa.input_validated) "true" else "false"});
        try writer.print("    \"piiContained\": {s},\n", .{if (fa.pii_contained) "true" else "false"});
        try writer.print("    \"injectionSafe\": {s}\n", .{if (fa.injection_safe) "true" else "false"});
        try writer.writeAll("  }");
    }

    // manifestAlignment (optional)
    if (report.manifest_alignment) |ma| {
        try writer.writeAll(",\n");
        try writer.writeAll("  \"manifestAlignment\": {\n");
        try writer.writeAll("    \"overall\": ");
        try writeJsonString(writer, ma.overall);
        try writer.writeAll(",\n");
        try writer.writeAll("    \"sections\": [");
        for (ma.sections, 0..) |section, i| {
            if (i > 0) try writer.writeAll(",");
            try writer.writeAll("\n      {\n");
            try writer.writeAll("        \"name\": ");
            try writeJsonString(writer, section.name);
            try writer.writeAll(",\n");
            try writer.writeAll("        \"status\": ");
            try writeJsonString(writer, section.status);
            try writer.writeAll(",\n");
            try writer.print("        \"declared\": {d},\n", .{section.declared});
            try writer.print("        \"matched\": {d}\n", .{section.matched});
            try writer.writeAll("      }");
        }
        if (ma.sections.len > 0) {
            try writer.writeAll("\n    ");
        }
        try writer.writeAll("]\n");
        try writer.writeAll("  }");
    }

    // propertyExpectations (optional)
    if (report.property_expectations) |pe| {
        try writer.writeAll(",\n");
        try writer.writeAll("  \"propertyExpectations\": {\n");
        try writer.print("    \"passed\": {s},\n", .{if (pe.passed) "true" else "false"});
        try writer.print("    \"checkedRoutes\": {d},\n", .{pe.checked_routes});
        try writer.print("    \"mismatches\": {d}\n", .{pe.mismatches});
        try writer.writeAll("  }");
    }

    if (report.integration) |integration| {
        try writer.writeAll(",\n");
        try writer.writeAll("  \"integration\": {\n");
        try writer.print("    \"generatorPack\": {s},\n", .{if (integration.generator_pack) "true" else "false"});
        try writer.print("    \"sqlSchema\": {s},\n", .{if (integration.sql_schema) "true" else "false"});
        try writer.print("    \"manifest\": {s},\n", .{if (integration.manifest) "true" else "false"});
        try writer.print("    \"propertyExpectations\": {s},\n", .{if (integration.property_expectations) "true" else "false"});
        try writer.print("    \"dataLabels\": {s},\n", .{if (integration.data_labels) "true" else "false"});
        try writer.print("    \"replay\": {s},\n", .{if (integration.replay) "true" else "false"});
        try writer.print("    \"faultSeverity\": {s}\n", .{if (integration.fault_severity) "true" else "false"});
        try writer.writeAll("  }");
    }

    try writer.writeAll("\n}\n");
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "writeReportJson: minimal report" {
    const allocator = std.testing.allocator;

    const report = BuildReport{
        .handler = "handler.ts",
        .proof_level = "none",
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeReportJson(&aw.writer, &report);
    output = aw.toArrayList();

    const json = output.items;
    try std.testing.expect(std.mem.indexOf(u8, json, "\"handler\": \"handler.ts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"proofLevel\": \"none\"") != null);
    // Optional sections should be absent
    try std.testing.expect(std.mem.indexOf(u8, json, "\"verification\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"properties\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"faultCoverage\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"flowAnalysis\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"manifestAlignment\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"propertyExpectations\"") == null);
}

test "writeReportJson: full report" {
    const allocator = std.testing.allocator;

    const checks = [_][]const u8{ "exhaustiveReturns", "resultsSafe", "bytecodeVerified" };
    const sections = [_]SectionResult{
        .{ .name = "routes", .status = "pass", .declared = 3, .matched = 3 },
        .{ .name = "env", .status = "warn", .declared = 2, .matched = 1 },
    };

    const report = BuildReport{
        .handler = "app/handler.ts",
        .proof_level = "complete",
        .verification = .{
            .passed = true,
            .checks = &checks,
        },
        .properties = .{
            .state_isolated = true,
            .injection_safe = true,
            .read_only = false,
            .pure = false,
            .stateless = false,
            .retry_safe = true,
            .deterministic = true,
            .idempotent = true,
            .fault_covered = true,
            .no_secret_leakage = true,
            .no_credential_leakage = true,
            .input_validated = true,
            .pii_contained = true,
        },
        .fault_coverage = .{
            .total_failable = 5,
            .covered = 5,
            .warnings = 0,
            .clean = true,
        },
        .flow_analysis = .{
            .no_secret_leakage = true,
            .no_credential_leakage = true,
            .input_validated = true,
            .pii_contained = true,
            .injection_safe = true,
        },
        .manifest_alignment = .{
            .overall = "pass",
            .sections = &sections,
        },
        .property_expectations = .{
            .passed = true,
            .checked_routes = 4,
            .mismatches = 0,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeReportJson(&aw.writer, &report);
    output = aw.toArrayList();

    const json = output.items;

    // Top-level fields
    try std.testing.expect(std.mem.indexOf(u8, json, "\"handler\": \"app/handler.ts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"proofLevel\": \"complete\"") != null);

    // Verification
    try std.testing.expect(std.mem.indexOf(u8, json, "\"passed\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"exhaustiveReturns\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"resultsSafe\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"bytecodeVerified\"") != null);

    // Properties
    try std.testing.expect(std.mem.indexOf(u8, json, "\"stateIsolated\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"readOnly\": false") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"idempotent\": true") != null);

    // Fault coverage
    try std.testing.expect(std.mem.indexOf(u8, json, "\"totalFailable\": 5") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"covered\": 5") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"clean\": true") != null);

    // Flow analysis
    try std.testing.expect(std.mem.indexOf(u8, json, "\"noSecretLeakage\": true") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"injectionSafe\": true") != null);

    // Manifest alignment
    try std.testing.expect(std.mem.indexOf(u8, json, "\"overall\": \"pass\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"name\": \"routes\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"name\": \"env\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"declared\": 3") != null);

    // Property expectations
    try std.testing.expect(std.mem.indexOf(u8, json, "\"checkedRoutes\": 4") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"mismatches\": 0") != null);
}

test "writeReportJson: partial sections" {
    const allocator = std.testing.allocator;

    const report = BuildReport{
        .handler = "handler.ts",
        .proof_level = "partial",
        .verification = .{
            .passed = false,
            .checks = &[_][]const u8{"exhaustiveReturns"},
        },
        .fault_coverage = .{
            .total_failable = 3,
            .covered = 1,
            .warnings = 2,
            .clean = false,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &output);

    try writeReportJson(&aw.writer, &report);
    output = aw.toArrayList();

    const json = output.items;

    // Present sections
    try std.testing.expect(std.mem.indexOf(u8, json, "\"verification\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"passed\": false") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"faultCoverage\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"warnings\": 2") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"clean\": false") != null);

    // Absent sections
    try std.testing.expect(std.mem.indexOf(u8, json, "\"properties\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"flowAnalysis\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"manifestAlignment\"") == null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"propertyExpectations\"") == null);
}

test "buildReport: from contract with verification and properties" {
    const allocator = std.testing.allocator;

    var contract = emptyContract();
    defer contract.deinit(allocator);

    contract.verification = .{
        .exhaustive_returns = true,
        .results_safe = true,
        .unreachable_code = false,
        .bytecode_verified = true,
    };

    contract.properties = .{
        .pure = false,
        .read_only = true,
        .stateless = false,
        .retry_safe = true,
        .deterministic = true,
        .has_egress = false,
        .state_isolated = true,
        .injection_safe = true,
        .idempotent = true,
        .fault_covered = false,
    };

    contract.fault_coverage = .{
        .total_failable = 2,
        .covered = 2,
        .warnings = 0,
    };

    const br = buildReport(allocator, &contract, null, null, null, "handler.ts");
    defer {
        if (br.verification) |v| allocator.free(v.checks);
    }

    try std.testing.expectEqualStrings("handler.ts", br.handler);
    // verification + properties present, no dynamic flags -> partial or complete
    try std.testing.expect(br.verification != null);
    try std.testing.expect(br.verification.?.passed);
    try std.testing.expect(br.properties != null);
    try std.testing.expect(br.properties.?.read_only);
    try std.testing.expect(!br.properties.?.pure);
    try std.testing.expect(br.fault_coverage != null);
    try std.testing.expectEqual(@as(u32, 2), br.fault_coverage.?.total_failable);
    try std.testing.expect(br.flow_analysis != null);
    try std.testing.expect(br.flow_analysis.?.injection_safe);
    try std.testing.expect(br.manifest_alignment == null);
    try std.testing.expect(br.property_expectations == null);
}

test "buildReport: minimal contract" {
    const allocator = std.testing.allocator;

    var contract = emptyContract();
    defer contract.deinit(allocator);

    const report = buildReport(allocator, &contract, null, null, null, "minimal.ts");

    try std.testing.expectEqualStrings("minimal.ts", report.handler);
    try std.testing.expectEqualStrings("none", report.proof_level);
    try std.testing.expect(report.verification == null);
    try std.testing.expect(report.properties == null);
    try std.testing.expect(report.fault_coverage == null);
    try std.testing.expect(report.flow_analysis == null);
}

fn emptyContract() HandlerContract {
    return .{
        .handler = .{ .path = &.{}, .line = 0, .column = 0 },
        .routes = .empty,
        .modules = .empty,
        .functions = .empty,
        .env = .{ .literal = .empty, .dynamic = false },
        .egress = .{ .hosts = .empty, .dynamic = false },
        .cache = .{ .namespaces = .empty, .dynamic = false },
        .sql = .{
            .backend = "sqlite",
            .queries = .empty,
            .dynamic = false,
        },
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
