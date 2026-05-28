//! pi_recall_facts - read back the project-scoped fact corpus.
//!
//! Companion to `pi_remember_fact`. Returns the persisted entries from
//! `<project_root>/.zigttp/memory.jsonl`, pinned-first then most-recent
//! unpinned, capped by an optional `limit`. The persona already injects a
//! budgeted selection at session start; this tool is for the agent to
//! reach into the rest of the corpus on demand (e.g. when the user
//! references something not in the injected slice).
//!
//! Input:
//!   { "limit"?: number, "pinned_only"?: bool }
//!
//! Output:
//!   { "ok": true, "total": N,
//!     "entries": [
//!       { "id": "...", "fact": "...", "source": "...",
//!         "session_id": "..."|null, "timestamp_unix_ms": 0, "pinned": false } ] }

const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const memory_store = @import("../memory_store.zig");
const json_utils = zigts.json_utils;

const name = "pi_recall_facts";
const default_limit: usize = 20;

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "recall-facts",
    .description =
    \\Read back the persisted project memory corpus from
    \\.zigttp/memory.jsonl. Pinned facts come first (chronological), then
    \\unpinned facts most-recent first. `limit` caps the response (default
    \\20). `pinned_only` filters to just the pinned set. A missing corpus
    \\is not an error: the response is { "ok": true, "total": 0,
    \\"entries": [] }.
    \\
    \\Call this when the user references project context the persona's
    \\injected slice may not have covered, or when you need to inspect
    \\everything the corpus knows about a topic.
    ,
    .input_schema =
    \\{"type":"object","properties":{"limit":{"type":"integer","minimum":1},"pinned_only":{"type":"boolean"}}}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

/// Decoder lays the optional fields into a fixed argv slot layout:
///   args[0] = limit as decimal string (always present; defaults to "20")
///   args[1] = pinned_only ("true" | "false"; defaults to "false")
fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    const trimmed = std.mem.trim(u8, args_json, " \t\r\n");
    var limit: usize = default_limit;
    var pinned_only: bool = false;

    if (trimmed.len > 0 and !std.mem.eql(u8, trimmed, "{}")) {
        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, trimmed, .{});
        defer parsed.deinit();
        if (parsed.value != .object) return error.InvalidToolArgsJson;
        const obj = parsed.value.object;
        if (obj.get("limit")) |lv| {
            if (lv != .integer or lv.integer <= 0) return error.InvalidToolArgsJson;
            limit = @intCast(lv.integer);
        }
        if (obj.get("pinned_only")) |pv| {
            if (pv != .bool) return error.InvalidToolArgsJson;
            pinned_only = pv.bool;
        }
    }

    const out = try allocator.alloc([]const u8, 2);
    errdefer allocator.free(out);
    out[0] = try std.fmt.allocPrint(allocator, "{d}", .{limit});
    out[1] = try allocator.dupe(u8, if (pinned_only) "true" else "false");
    return out;
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    const limit: usize = blk: {
        if (args.len < 1) break :blk default_limit;
        break :blk std.fmt.parseInt(usize, args[0], 10) catch default_limit;
    };
    const pinned_only: bool = args.len >= 2 and std.mem.eql(u8, args[1], "true");

    const root = common.workspaceRoot(allocator) catch |err| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to resolve workspace root: {s}\n",
            .{@errorName(err)},
        );
    };
    defer allocator.free(root);

    const entries = memory_store.loadAll(allocator, root) catch |err| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to load memory store: {s}\n",
            .{@errorName(err)},
        );
    };
    defer memory_store.freeEntries(allocator, entries);

    // Reuse the persona selector for ordering (pinned chronological, then
    // unpinned most-recent first) by passing a budget large enough that the
    // cap is purely `limit`, not bytes.
    const selection = try memory_store.selectForPersona(allocator, entries, std.math.maxInt(usize));
    defer allocator.free(selection);

    var kept: std.ArrayList(*const memory_store.Entry) = .empty;
    defer kept.deinit(allocator);
    for (selection) |e| {
        if (kept.items.len >= limit) break;
        if (pinned_only and !e.pinned) continue;
        try kept.append(allocator, e);
    }

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &out);
    const w = &aw.writer;

    try w.print("{{\"ok\":true,\"total\":{d},\"entries\":[", .{kept.items.len});
    for (kept.items, 0..) |e, i| {
        if (i > 0) try w.writeByte(',');
        try w.writeAll("{\"id\":");
        try json_utils.writeJsonString(w, e.id);
        try w.writeAll(",\"fact\":");
        try json_utils.writeJsonString(w, e.fact);
        try w.writeAll(",\"source\":");
        try json_utils.writeJsonString(w, e.source);
        try w.writeAll(",\"session_id\":");
        if (e.session_id) |sid| {
            try json_utils.writeJsonString(w, sid);
        } else {
            try w.writeAll("null");
        }
        try w.print(
            ",\"timestamp_unix_ms\":{d},\"pinned\":{}}}",
            .{ e.timestamp_unix_ms, e.pinned },
        );
    }
    try w.writeAll("]}\n");
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
    try testing.expectEqualStrings("pi_recall_facts", tool.name);
    try testing.expectEqualStrings("recall-facts", tool.label);
}

test "decodeJson accepts an empty object and applies defaults" {
    const allocator = testing.allocator;
    const args = try decodeJson(allocator, "{}");
    defer {
        for (args) |a| allocator.free(a);
        allocator.free(args);
    }
    try testing.expectEqualStrings("20", args[0]);
    try testing.expectEqualStrings("false", args[1]);
}

test "decodeJson rejects non-positive limit" {
    const allocator = testing.allocator;
    try testing.expectError(error.InvalidToolArgsJson, decodeJson(allocator, "{\"limit\":0}"));
    try testing.expectError(error.InvalidToolArgsJson, decodeJson(allocator, "{\"limit\":-1}"));
}

test "decodeJson carries pinned_only" {
    const allocator = testing.allocator;
    const args = try decodeJson(allocator, "{\"limit\":5,\"pinned_only\":true}");
    defer {
        for (args) |a| allocator.free(a);
        allocator.free(args);
    }
    try testing.expectEqualStrings("5", args[0]);
    try testing.expectEqualStrings("true", args[1]);
}

test "execute on missing corpus returns total:0" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const args = [_][]const u8{ "20", "false" };
    var result = try execute(allocator, &args);
    defer result.deinit(allocator);
    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"total\":0") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"entries\":[]") != null);
}

test "execute returns persisted entries pinned-first then most-recent unpinned" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);

    try memory_store.append(allocator, root, .{
        .id = "a",
        .fact = "fact-a",
        .source = "tool",
        .timestamp_unix_ms = 1,
        .pinned = false,
    });
    try memory_store.append(allocator, root, .{
        .id = "b",
        .fact = "fact-b",
        .source = "manual_entry",
        .timestamp_unix_ms = 2,
        .pinned = true,
    });
    try memory_store.append(allocator, root, .{
        .id = "c",
        .fact = "fact-c",
        .source = "tool",
        .timestamp_unix_ms = 3,
        .pinned = false,
    });

    const args = [_][]const u8{ "20", "false" };
    var result = try execute(allocator, &args);
    defer result.deinit(allocator);
    try testing.expect(result.ok);
    const text = result.llm_text;
    try testing.expect(std.mem.indexOf(u8, text, "\"total\":3") != null);

    // Pinned ("b") must appear before the most-recent unpinned ("c"), which
    // must appear before the oldest unpinned ("a").
    const pos_b = std.mem.indexOf(u8, text, "\"id\":\"b\"") orelse return error.TestExpected;
    const pos_c = std.mem.indexOf(u8, text, "\"id\":\"c\"") orelse return error.TestExpected;
    const pos_a = std.mem.indexOf(u8, text, "\"id\":\"a\"") orelse return error.TestExpected;
    try testing.expect(pos_b < pos_c);
    try testing.expect(pos_c < pos_a);
}

test "execute pinned_only filters out unpinned entries" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    try memory_store.append(allocator, root, .{
        .id = "a",
        .fact = "fact-a",
        .source = "tool",
        .timestamp_unix_ms = 1,
        .pinned = false,
    });
    try memory_store.append(allocator, root, .{
        .id = "b",
        .fact = "fact-b",
        .source = "manual_entry",
        .timestamp_unix_ms = 2,
        .pinned = true,
    });

    const args = [_][]const u8{ "20", "true" };
    var result = try execute(allocator, &args);
    defer result.deinit(allocator);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"total\":1") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "fact-b") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "fact-a") == null);
}
