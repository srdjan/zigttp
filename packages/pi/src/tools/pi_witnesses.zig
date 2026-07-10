//! pi_witnesses - inspect the on-disk witness corpus for a handler.
//!
//! Lets the agent see how thinly defended each declared `Spec<...>` is and
//! pick up summaries of past falsifying inputs without leaving the
//! conversation. Use before drafting a repair: a Spec with no witnesses
//! is unprobed; a Spec with many pinned witnesses is the load-bearing one
//! and a repair must not weaken its coverage.
//!
//! Input:
//!   { "path": "handler.ts" }
//!
//! Output:
//!   { "ok": true,
//!     "handler_path": "handler.ts",
//!     "total": 3,
//!     "by_property": { "injection_safe": 1, "no_secret_leakage": 2 },
//!     "entries": [
//!       { "key": "abc...", "property": "no_secret_leakage",
//!         "summary": "SECRET_KEY flows into Response body",
//!         "pinned": false, "first_seen_unix_s": 1745539200 } ] }
//!
//! Errors are returned as `{"ok":false,"error":"..."}` with a non-zero
//! tool exit. A missing corpus is not an error: it returns total:0.

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const json_utils = zigts.json_utils;
const witness_corpus = zigts.witness_corpus;

const name = "pi_witnesses";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "witnesses",
    .effect = .read_workspace,
    .description =
    \\List the on-disk witness corpus for a handler: every persisted
    \\counterexample input that some property at some site is known to
    \\fail under, plus per-property counts for coverage triage.
    \\
    \\Use this before pi_repair_plan: source-declared Spec<...> tells you
    \\which obligations the author cares about; this tool tells you which
    \\of those obligations already have defending evidence and which are
    \\unprobed. Pinned entries are load-bearing: a repair that removes
    \\them weakens coverage and must not be applied without justification.
    \\
    \\A missing corpus is not an error: the response simply reports
    \\total:0. The corpus populates as pi_repair_plan and pi_goal_check
    \\materialise witnesses against the handler.
    ,
    .input_schema = "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeSingleStringField(allocator, args_json, "path");
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, name ++ ": requires a path\n");
    }

    const handler_path = args[0];
    const corpus_dir = zigts.witness_corpus.corpusDir(allocator, handler_path) catch {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to compute corpus directory for {s}\n",
            .{handler_path},
        );
    };
    defer allocator.free(corpus_dir);

    const entries = witness_corpus.loadEntries(allocator, corpus_dir) catch |err| switch (err) {
        error.WitnessCorpusMissing => {
            return try emitEmpty(allocator, handler_path);
        },
        else => return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to load corpus: {s}\n",
            .{@errorName(err)},
        ),
    };
    defer witness_corpus.freeEntries(allocator, entries);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &out);
    const w = &aw.writer;

    try w.writeAll("{\"ok\":true,\"handler_path\":");
    try json_utils.writeJsonString(w, handler_path);
    try w.print(",\"total\":{d},\"by_property\":{{", .{entries.len});

    const counts = try witness_corpus.countByProperty(allocator, corpus_dir);
    defer witness_corpus.freeCounts(allocator, counts);
    for (counts, 0..) |c, i| {
        if (i > 0) try w.writeByte(',');
        try json_utils.writeJsonString(w, c.property);
        try w.print(":{d}", .{c.count});
    }
    try w.writeAll("},\"entries\":[");

    for (entries, 0..) |e, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"key\":");
        try json_utils.writeJsonString(w, e.key);
        try w.writeAll(",\"property\":");
        try json_utils.writeJsonString(w, e.property);
        try w.writeAll(",\"summary\":");
        try json_utils.writeJsonString(w, e.summary);
        try w.print(
            ",\"pinned\":{},\"first_seen_unix_s\":{d}}}",
            .{ e.pinned, e.first_seen_unix_s },
        );
    }
    try w.writeAll("]}\n");
    out = aw.toArrayList();

    const text = try out.toOwnedSlice(allocator);
    defer allocator.free(text);
    return try registry_mod.ToolResult.withPlainText(allocator, true, text);
}

fn emitEmpty(
    allocator: std.mem.Allocator,
    handler_path: []const u8,
) anyerror!registry_mod.ToolResult {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &out);
    const w = &aw.writer;

    try w.writeAll("{\"ok\":true,\"handler_path\":");
    try json_utils.writeJsonString(w, handler_path);
    try w.writeAll(",\"total\":0,\"by_property\":{},\"entries\":[]}\n");
    out = aw.toArrayList();

    const text = try out.toOwnedSlice(allocator);
    defer allocator.free(text);
    return try registry_mod.ToolResult.withPlainText(allocator, true, text);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "tool registers expected name and label" {
    try testing.expectEqualStrings("pi_witnesses", tool.name);
    try testing.expectEqualStrings("witnesses", tool.label);
    try testing.expect(std.mem.indexOf(u8, tool.description, "corpus") != null);
    try testing.expect(std.mem.indexOf(u8, tool.description, "Spec<") != null);
}

test "execute on missing corpus emits zero-total response" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const args = [_][]const u8{"ghost.ts"};
    var result = try execute(allocator, &args);
    defer result.deinit(allocator);

    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"total\":0") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"by_property\":{}") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"entries\":[]") != null);
}

test "execute lists persisted entries with summary and counts" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    // Seed the corpus directly via the witness_corpus API so the test does
    // not depend on flow_checker producing a witness from sample source.
    const counterexample = zigts.counterexample;

    const dir = try witness_corpus.corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try witness_corpus.ensureCorpusDir(allocator, dir, "h.ts");

    var w = try counterexample.solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 2, .column = 1 },
        .origin_node_id = 7,
        .sink_node_id = 9,
        .summary = "leak summary",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer w.deinit(allocator);
    var pres = try witness_corpus.persist(allocator, dir, w);
    pres.deinit(allocator);

    const args = [_][]const u8{"h.ts"};
    var result = try execute(allocator, &args);
    defer result.deinit(allocator);

    try testing.expect(result.ok);
    const text = result.llm_text;
    try testing.expect(std.mem.indexOf(u8, text, "\"total\":1") != null);
    try testing.expect(std.mem.indexOf(u8, text, "\"no_secret_leakage\":1") != null);
    try testing.expect(std.mem.indexOf(u8, text, "leak summary") != null);
    try testing.expect(std.mem.indexOf(u8, text, "\"pinned\":false") != null);
}
