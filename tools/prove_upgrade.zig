//! Proven Evolution Pipeline
//!
//! Orchestrates the full proof pipeline that compares two handler versions:
//! 1. Parse old contract from JSON file
//! 2. Diff old contract against new contract (already built by precompile)
//! 3. Optionally run replay verification against traces
//! 4. Classify upgrade as equivalent/additive/breaking
//! 5. Emit proof.json + proof-report.txt
//!
//! Called by precompile.zig when --prove is passed.

const std = @import("std");
const zts = @import("zts");
const handler_contract = zts.handler_contract;
const HandlerContract = handler_contract.HandlerContract;
const contract_diff = zts.contract_diff;
const ContractDiff = contract_diff.ContractDiff;
const ProofCertificate = contract_diff.ProofCertificate;
pub const ReplaySummary = contract_diff.ReplaySummary;
const precompile = @import("precompile.zig");

pub const ProveResult = struct {
    certificate: ProofCertificate,
    /// Kept alive because the diff and certificate contain slices into it.
    old_contract: handler_contract.HandlerContract,

    pub fn deinit(self: *ProveResult, allocator: std.mem.Allocator) void {
        self.certificate.deinit(allocator);
        self.old_contract.deinit(allocator);
    }
};

/// Run the proven evolution pipeline.
///
/// old_contract_json: raw bytes of the old contract.json
/// new_contract: the new handler's contract (already built)
/// replay_summary: optional replay results (from runBuildTimeReplay)
/// new_handler_path: path string for the new handler
pub fn prove(
    allocator: std.mem.Allocator,
    old_contract_json: []const u8,
    new_contract: *const HandlerContract,
    replay_summary: ?ReplaySummary,
    new_handler_path: []const u8,
) !ProveResult {
    // Step 1: Parse old contract from JSON.
    // The old contract is kept alive for the duration since the diff
    // contains slices pointing into its data (route patterns, env vars).
    var old_contract = try handler_contract.parseFromJson(allocator, old_contract_json);
    // Note: NOT deferred - ownership transfers to ProveResult for later cleanup

    // Step 2: Structural diff
    const diff = try contract_diff.diffContracts(allocator, &old_contract, new_contract);

    // Step 3: Classify
    const classification = if (replay_summary) |rs|
        diff.classifyWithReplay(&rs)
    else
        diff.classify();

    // Step 4: Derive proof level from new contract
    const proof_level = contract_diff.deriveProofLevel(new_contract);

    // Step 5: Generate recommendation
    const recommendation = try contract_diff.generateRecommendation(
        allocator,
        classification,
        &diff,
        if (replay_summary) |*rs| rs else null,
    );

    return .{
        .certificate = .{
            .classification = classification,
            .old_handler = old_contract.handler.path,
            .new_handler = new_handler_path,
            .diff = diff,
            .replay = replay_summary,
            .proof_level = proof_level,
            .recommendation = recommendation,
        },
        .old_contract = old_contract,
    };
}

/// Write proof certificate outputs (proof.json + proof-report.txt).
pub fn writeProofOutputs(
    allocator: std.mem.Allocator,
    cert: *const ProofCertificate,
    output_dir: []const u8,
) !void {
    // Write proof.json
    {
        var json_output: std.ArrayList(u8) = .empty;
        defer json_output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &json_output);

        try contract_diff.writeProofJson(cert, &aw.writer);
        json_output = aw.toArrayList();

        const json_path = try std.fmt.allocPrint(allocator, "{s}proof.json", .{output_dir});
        defer allocator.free(json_path);

        try precompile.writeFilePosix(json_path, json_output.items, allocator);
        std.debug.print("Wrote proof certificate to: {s}\n", .{json_path});
    }

    // Write proof-report.txt
    {
        var report_output: std.ArrayList(u8) = .empty;
        defer report_output.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &report_output);

        try contract_diff.writeProofReport(&aw.writer, cert);
        report_output = aw.toArrayList();

        const report_path = try std.fmt.allocPrint(allocator, "{s}proof-report.txt", .{output_dir});
        defer allocator.free(report_path);

        try precompile.writeFilePosix(report_path, report_output.items, allocator);
        std.debug.print("Wrote proof report to: {s}\n", .{report_path});
    }
}
