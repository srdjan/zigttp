//! pi_remember_fact - persist a project-scoped fact for cross-session recall.
//!
//! The agent uses this to record durable facts about the project (naming
//! conventions, "we tried X and it failed because Y", load-bearing
//! invariants, etc.) so a future session - rebuilt from a binary snapshot,
//! with no in-process memory - can still see them. Persona injection
//! surfaces a budgeted selection at session start; `pi_recall_facts`
//! retrieves the full corpus on demand.
//!
//! Input:
//!   { "fact": "string", "source"?: "string", "pinned"?: bool }
//!
//! Output:
//!   { "ok": true, "id": "26-char-id" }
//!
//! Storage: `<project_root>/.zigttp/memory.jsonl`, append-only JSONL.

const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const memory_store = @import("../memory_store.zig");
const session_id_mod = @import("../session/session_id.zig");

const name = "pi_remember_fact";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "remember-fact",
    .description =
    \\Persist a project-scoped fact to .zigttp/memory.jsonl so it survives
    \\across expert sessions. Use this for load-bearing observations the
    \\next session would otherwise have to rediscover: naming conventions,
    \\failed approaches and why they failed, invariants the test suite
    \\protects, links to specific files. Pinned facts are always injected
    \\into the persona at session start; unpinned facts are injected
    \\most-recent-first when the budget allows.
    \\
    \\Do not use this for transient state (current goal, current file)
    \\or for anything secret-bearing - facts are read-only project memory,
    \\not a notepad.
    ,
    .input_schema =
    \\{"type":"object","properties":{"fact":{"type":"string"},"source":{"type":"string"},"pinned":{"type":"boolean"}},"required":["fact"]}
    ,
    .decode_json = decodeJson,
    .execute = execute,
};

/// Decoder packs the optional fields into a fixed argv layout the executor
/// reads positionally:
///   args[0] = fact (required)
///   args[1] = source (always present; defaults to "tool" when omitted)
///   args[2] = pinned ("true" | "false"; defaults to "false")
fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, args_json, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidToolArgsJson;
    const obj = parsed.value.object;

    const fact_v = obj.get("fact") orelse return error.InvalidToolArgsJson;
    if (fact_v != .string) return error.InvalidToolArgsJson;
    if (fact_v.string.len == 0) return error.InvalidToolArgsJson;

    var source: []const u8 = "tool";
    if (obj.get("source")) |sv| {
        if (sv != .string) return error.InvalidToolArgsJson;
        if (sv.string.len > 0) source = sv.string;
    }

    var pinned_str: []const u8 = "false";
    if (obj.get("pinned")) |pv| {
        if (pv != .bool) return error.InvalidToolArgsJson;
        pinned_str = if (pv.bool) "true" else "false";
    }

    const out = try allocator.alloc([]const u8, 3);
    errdefer allocator.free(out);
    out[0] = try allocator.dupe(u8, fact_v.string);
    errdefer allocator.free(out[0]);
    out[1] = try allocator.dupe(u8, source);
    errdefer allocator.free(out[1]);
    out[2] = try allocator.dupe(u8, pinned_str);
    return out;
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, name ++ ": requires a fact\n");
    }

    const fact = args[0];
    if (fact.len == 0) {
        return registry_mod.ToolResult.err(allocator, name ++ ": fact must not be empty\n");
    }
    const source: []const u8 = if (args.len >= 2) args[1] else "tool";
    const pinned: bool = args.len >= 3 and std.mem.eql(u8, args[2], "true");

    const root = common.workspaceRoot(allocator) catch |err| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to resolve workspace root: {s}\n",
            .{@errorName(err)},
        );
    };
    defer allocator.free(root);

    const id = session_id_mod.generate(allocator) catch |err| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to generate id: {s}\n",
            .{@errorName(err)},
        );
    };
    defer allocator.free(id);

    memory_store.append(allocator, root, .{
        .id = id,
        .fact = fact,
        .source = source,
        .session_id = null,
        .timestamp_unix_ms = common.nowUnixMs(),
        .pinned = pinned,
    }) catch |err| {
        return registry_mod.ToolResult.errFmt(
            allocator,
            name ++ ": failed to persist fact: {s}\n",
            .{@errorName(err)},
        );
    };

    const text = try std.fmt.allocPrint(
        allocator,
        "{{\"ok\":true,\"id\":\"{s}\"}}\n",
        .{id},
    );
    defer allocator.free(text);
    return try registry_mod.ToolResult.withPlainText(allocator, true, text);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "tool registers expected name, label, and required fields" {
    try testing.expectEqualStrings("pi_remember_fact", tool.name);
    try testing.expectEqualStrings("remember-fact", tool.label);
    try testing.expect(std.mem.indexOf(u8, tool.input_schema, "\"fact\"") != null);
    try testing.expect(std.mem.indexOf(u8, tool.input_schema, "\"required\":[\"fact\"]") != null);
}

test "decodeJson rejects missing or empty fact" {
    const allocator = testing.allocator;
    try testing.expectError(error.InvalidToolArgsJson, decodeJson(allocator, "{}"));
    try testing.expectError(error.InvalidToolArgsJson, decodeJson(allocator, "{\"fact\":\"\"}"));
    try testing.expectError(error.InvalidToolArgsJson, decodeJson(allocator, "{\"fact\":42}"));
}

test "decodeJson supplies tool/source and false/pinned defaults" {
    const allocator = testing.allocator;
    const args = try decodeJson(allocator, "{\"fact\":\"hello\"}");
    defer {
        for (args) |a| allocator.free(a);
        allocator.free(args);
    }
    try testing.expectEqual(@as(usize, 3), args.len);
    try testing.expectEqualStrings("hello", args[0]);
    try testing.expectEqualStrings("tool", args[1]);
    try testing.expectEqualStrings("false", args[2]);
}

test "decodeJson carries explicit source and pinned values" {
    const allocator = testing.allocator;
    const args = try decodeJson(
        allocator,
        "{\"fact\":\"x\",\"source\":\"manual_entry\",\"pinned\":true}",
    );
    defer {
        for (args) |a| allocator.free(a);
        allocator.free(args);
    }
    try testing.expectEqualStrings("manual_entry", args[1]);
    try testing.expectEqualStrings("true", args[2]);
}

test "execute persists a fact under the current workspace root" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    defer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const args = [_][]const u8{ "first fact", "manual_entry", "true" };
    var result = try execute(allocator, &args);
    defer result.deinit(allocator);
    try testing.expect(result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"ok\":true") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"id\":\"") != null);

    // Round-trip via memory_store to confirm the entry actually landed.
    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const entries = try memory_store.loadAll(allocator, root);
    defer memory_store.freeEntries(allocator, entries);
    try testing.expectEqual(@as(usize, 1), entries.len);
    try testing.expectEqualStrings("first fact", entries[0].fact);
    try testing.expectEqualStrings("manual_entry", entries[0].source);
    try testing.expect(entries[0].pinned);
}
