//! Standalone Contract Proof Tool
//!
//! Compares two handler contract.json files and classifies the change as
//! equivalent, additive, or breaking. Outputs proof.json and proof-report.txt.
//!
//! Usage: zig build prove -- <old-contract.json> <new-contract.json> [output-dir/]
//!
//! Exit codes: 0 = equivalent or additive, 1 = breaking, 2 = error.

const std = @import("std");
const zts = @import("zts");
const handler_contract = zts.handler_contract;
const contract_diff = zts.contract_diff;
const readFilePosix = zts.file_io.readFile;

pub fn main(init: std.process.Init.Minimal) !void {
    const allocator = std.heap.smp_allocator;

    var args_iter = std.process.Args.Iterator.init(init.args);
    _ = args_iter.next(); // skip program name

    const old_path = args_iter.next() orelse {
        std.debug.print("Usage: prove <old-contract.json> <new-contract.json> [output-dir/]\n", .{});
        std.process.exit(2);
    };
    const new_path = args_iter.next() orelse {
        std.debug.print("Usage: prove <old-contract.json> <new-contract.json> [output-dir/]\n", .{});
        std.process.exit(2);
    };
    const output_dir: []const u8 = args_iter.next() orelse "./";

    // Read both contract files
    const old_json = readFilePosix(allocator, old_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ old_path, err });
        std.process.exit(2);
    };
    defer allocator.free(old_json);

    const new_json = readFilePosix(allocator, new_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ new_path, err });
        std.process.exit(2);
    };
    defer allocator.free(new_json);

    // Parse both contracts
    var old_contract = handler_contract.parseFromJson(allocator, old_json) catch |err| {
        std.debug.print("Error parsing {s}: {}\n", .{ old_path, err });
        std.process.exit(2);
    };
    defer old_contract.deinit(allocator);

    var new_contract = handler_contract.parseFromJson(allocator, new_json) catch |err| {
        std.debug.print("Error parsing {s}: {}\n", .{ new_path, err });
        std.process.exit(2);
    };
    defer new_contract.deinit(allocator);

    // Diff and classify
    const diff = try contract_diff.diffContracts(allocator, &old_contract, &new_contract);
    const classification = diff.classify();
    const proof_level = contract_diff.deriveProofLevel(&new_contract);
    const recommendation = try contract_diff.generateRecommendation(allocator, classification, &diff, null);
    defer allocator.free(recommendation);

    const cert = contract_diff.ProofCertificate{
        .classification = classification,
        .old_handler = old_contract.handler.path,
        .new_handler = new_contract.handler.path,
        .diff = diff,
        .replay = null,
        .proof_level = proof_level,
        .recommendation = recommendation,
    };

    // Write proof.json
    {
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        try contract_diff.writeProofJson(&cert, &aw.writer);
        buf = aw.toArrayList();

        const path = try std.fmt.allocPrint(allocator, "{s}proof.json", .{output_dir});
        defer allocator.free(path);
        try zts.file_io.writeFile(path, buf.items, allocator);
        std.debug.print("Wrote {s}\n", .{path});
    }

    // Write proof-report.txt
    {
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        try contract_diff.writeProofReport(&aw.writer, &cert);
        buf = aw.toArrayList();

        const path = try std.fmt.allocPrint(allocator, "{s}proof-report.txt", .{output_dir});
        defer allocator.free(path);
        try zts.file_io.writeFile(path, buf.items, allocator);
        std.debug.print("Wrote {s}\n", .{path});
    }

    std.debug.print("Classification: {s}\n", .{classification.toString()});
    std.debug.print("Proof level: {s}\n", .{proof_level.toString()});

    if (classification == .breaking) {
        std.process.exit(1);
    }
}

