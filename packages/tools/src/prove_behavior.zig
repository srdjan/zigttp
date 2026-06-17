//! `zigts prove-behavior <before.ts> <after.ts> [--json]` subcommand.
//!
//! Proof-carrying changes: compiles two handler versions, runs the same
//! contract-diff the expert loop computes on every edit, and reports the
//! behavioral-equivalence verdict - equivalent, equivalent_modulo_laws,
//! additive, or breaking - plus the per-path behavior delta.
//!
//! Unlike `prove`, which diffs two pre-extracted contract.json files, this
//! takes two TypeScript sources directly so it is usable as a one-shot CI
//! gate ("did this edit change what the handler does?"). It emits the
//! verdict unsigned. It does not append to the proof ledger; callers that need
//! a durable signed receipt should wrap this verdict explicitly.
//!
//! Exit codes mirror `prove`: 0 = equivalent / equivalent_modulo_laws /
//! additive (safe), 1 = breaking, 2 = usage or compile error.

const std = @import("std");
const zigts = @import("zigts");
const edit_simulate = @import("edit_simulate.zig");
const precompile = @import("precompile.zig");

const contract_diff = zigts.contract_diff;
const file_io = zigts.file_io;

const max_source_bytes = 4 * 1024 * 1024;

/// The verdict, decoupled from how it is rendered or signed. `laws_fired`
/// and the strings borrow the diff/arena; valid until the caller frees them.
pub const Verdict = struct {
    classification: []const u8,
    claim_scope: []const u8,
    preserved: u32,
    response_changed: u32,
    added: u32,
    removed: u32,
};

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var json_mode = false;
    var explicit_sql_schema_path: ?[]const u8 = null;
    var before_path: ?[]const u8 = null;
    var after_path: ?[]const u8 = null;

    var i: usize = 0;
    while (i < argv.len) : (i += 1) {
        const arg = argv[i];
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
            continue;
        }
        if (std.mem.eql(u8, arg, "--sql-schema")) {
            i += 1;
            if (i >= argv.len) return error.MissingArgument;
            explicit_sql_schema_path = argv[i];
            continue;
        }
        if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        }
        if (std.mem.startsWith(u8, arg, "-")) {
            std.debug.print("Unknown flag: {s}\n", .{arg});
            printHelp();
            std.process.exit(2);
        }
        if (before_path == null) {
            before_path = arg;
        } else if (after_path == null) {
            after_path = arg;
        } else {
            std.debug.print("Unexpected argument: {s}\n", .{arg});
            printHelp();
            std.process.exit(2);
        }
    }

    const before = before_path orelse {
        printHelp();
        std.process.exit(2);
    };
    const after = after_path orelse {
        printHelp();
        std.process.exit(2);
    };

    const discovered_schema = if (explicit_sql_schema_path == null)
        edit_simulate.discoverProjectSqlSchemaPath(allocator, after) orelse
            edit_simulate.discoverProjectSqlSchemaPath(allocator, before)
    else
        null;
    defer if (discovered_schema) |path| allocator.free(path);
    const sql_schema_path = explicit_sql_schema_path orelse discovered_schema;

    const before_src = file_io.readFile(allocator, before, max_source_bytes) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ before, err });
        std.process.exit(2);
    };
    defer allocator.free(before_src);

    const after_src = file_io.readFile(allocator, after, max_source_bytes) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ after, err });
        std.process.exit(2);
    };
    defer allocator.free(after_src);

    var before_result = try precompile.runCheckOnlyFromSource(allocator, before_src, before, sql_schema_path, true, null, false);
    defer before_result.deinit(allocator);
    var after_result = try precompile.runCheckOnlyFromSource(allocator, after_src, after, sql_schema_path, true, null, false);
    defer after_result.deinit(allocator);

    const before_contract = if (before_result.contract) |*c| c else {
        std.debug.print("Error: could not extract a contract from {s}\n", .{before});
        std.process.exit(2);
    };
    const after_contract = if (after_result.contract) |*c| c else {
        std.debug.print("Error: could not extract a contract from {s}\n", .{after});
        std.process.exit(2);
    };

    var diff = try contract_diff.diffContracts(allocator, before_contract, after_contract);
    defer diff.deinit(allocator);
    const classification = diff.behavioralVerdict();

    const scope = contract_diff.claimScope(after_contract.properties);
    const bd = diff.behavior_diff;

    if (json_mode) {
        try writeJson(allocator, classification, scope, bd, diff.laws_used.items);
    } else {
        try writeText(allocator, classification, scope, bd, diff.laws_used.items);
    }

    if (classification == .breaking) std.process.exit(1);
}

fn writeText(
    allocator: std.mem.Allocator,
    classification: contract_diff.Classification,
    scope: []const u8,
    behavior_diff: ?contract_diff.BehaviorDiff,
    laws: []const []const u8,
) !void {
    std.debug.print("classification: {s}\n", .{classification.toString()});
    std.debug.print("claim_scope:    {s}\n", .{scope});
    if (behavior_diff) |bd| {
        std.debug.print(
            "behavior:       preserved={d} response_changed={d} added={d} removed={d}\n",
            .{ bd.preserved, bd.response_changed, bd.added, bd.removed },
        );
    }
    if (laws.len > 0) {
        var joined: std.ArrayList(u8) = .empty;
        defer joined.deinit(allocator);
        for (laws, 0..) |law, i| {
            if (i > 0) try joined.appendSlice(allocator, ", ");
            try joined.appendSlice(allocator, law);
        }
        std.debug.print("laws_fired:     {s}\n", .{joined.items});
    }
}

fn writeJson(
    allocator: std.mem.Allocator,
    classification: contract_diff.Classification,
    scope: []const u8,
    behavior_diff: ?contract_diff.BehaviorDiff,
    laws: []const []const u8,
) !void {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    var json: std.json.Stringify = .{ .writer = &aw.writer };
    try json.beginObject();
    try json.objectField("classification");
    try json.write(classification.toString());
    try json.objectField("claimScope");
    try json.write(scope);
    try json.objectField("safe");
    try json.write(classification != .breaking);
    try json.objectField("behaviorDiff");
    if (behavior_diff) |bd| {
        try json.beginObject();
        try json.objectField("preserved");
        try json.write(bd.preserved);
        try json.objectField("responseChanged");
        try json.write(bd.response_changed);
        try json.objectField("added");
        try json.write(bd.added);
        try json.objectField("removed");
        try json.write(bd.removed);
        try json.endObject();
    } else {
        try json.write(null);
    }
    try json.objectField("lawsFired");
    try json.beginArray();
    for (laws) |law| try json.write(law);
    try json.endArray();
    try json.endObject();
    try aw.writer.writeByte('\n');

    const bytes = aw.writer.buffered();
    if (bytes.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
    }
}

fn printHelp() void {
    const help =
        \\zigts prove-behavior - behavioral-equivalence verdict between two handler versions
        \\
        \\Usage: zigts prove-behavior <before.ts> <after.ts> [--json] [--sql-schema path]
        \\
        \\Compiles both versions, runs the contract diff, and reports whether the
        \\edit is behaviorally equivalent / equivalent-modulo-laws / additive /
        \\breaking, with the per-path behavior delta and the laws the equivalence
        \\proof relied on. The verdict is unsigned and is not appended to the
        \\proof ledger.
        \\
        \\Exit codes: 0 = safe (equivalent / additive), 1 = breaking, 2 = error.
        \\
    ;
    std.debug.print("{s}", .{help});
}

// behavioralVerdict and claimScope live in `contract_diff` (shared with the
// equivalence receipt); their unit tests live there too.
