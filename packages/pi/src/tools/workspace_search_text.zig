const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

const name = "workspace_search_text";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "search text",
    .description = "Search the workspace for a text substring and return path/line matches.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\"},\"path\":{\"type\":\"string\"},\"limit\":{\"type\":\"integer\",\"minimum\":1}},\"required\":[\"query\"]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

/// Resolved invocation args. When the input is JSON, `query`/`path` point into
/// `owned_query`/`owned_path`, which the caller frees via `deinit`. When the
/// input is positional, they alias the caller-owned `args` slices and the owned
/// buffers are null. Either way the slices outlive the JSON parse tree.
const ParsedArgs = struct {
    query: []const u8,
    path: []const u8,
    limit: usize,
    owned_query: ?[]u8 = null,
    owned_path: ?[]u8 = null,

    fn deinit(self: *const ParsedArgs, allocator: std.mem.Allocator) void {
        if (self.owned_query) |q| allocator.free(q);
        if (self.owned_path) |p| allocator.free(p);
    }
};

const ParseResult = union(enum) {
    ok: ParsedArgs,
    /// A structured error to hand straight back to the caller (owns its text).
    err: registry_mod.ToolResult,
};

/// Parse and validate the tool input. JSON-derived strings are duped only after
/// every field validates, so no error path leaks an allocation, and the duped
/// query/path outlive `parsed.deinit()` (the use-after-free fixed in ae881b6).
fn parseArgs(allocator: std.mem.Allocator, args: []const []const u8) !ParseResult {
    if (args.len > 0 and args[0].len > 0 and args[0][0] == '{') {
        var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
            return .{ .err = try registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n") };
        };
        defer parsed.deinit();
        if (parsed.value != .object) return .{ .err = try registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n") };
        const obj = parsed.value.object;
        const query_val = obj.get("query") orelse return .{ .err = try registry_mod.ToolResult.err(allocator, name ++ ": missing query\n") };
        if (query_val != .string) return .{ .err = try registry_mod.ToolResult.err(allocator, name ++ ": query must be a string\n") };

        var path_str: ?[]const u8 = null;
        if (obj.get("path")) |value| {
            if (value != .string) return .{ .err = try registry_mod.ToolResult.err(allocator, name ++ ": path must be a string\n") };
            path_str = value.string;
        }
        var limit: usize = 50;
        if (obj.get("limit")) |value| {
            if (value != .integer or value.integer <= 0) return .{ .err = try registry_mod.ToolResult.err(allocator, name ++ ": limit must be a positive integer\n") };
            limit = @intCast(value.integer);
        }

        // Everything validated: dupe the strings out of the parse tree before it
        // is torn down on return.
        const owned_query = try allocator.dupe(u8, query_val.string);
        errdefer allocator.free(owned_query);
        const owned_path: ?[]u8 = if (path_str) |ps| try allocator.dupe(u8, ps) else null;

        return .{ .ok = .{
            .query = owned_query,
            .path = owned_path orelse ".",
            .limit = limit,
            .owned_query = owned_query,
            .owned_path = owned_path,
        } };
    } else if (args.len > 0) {
        return .{ .ok = .{
            .query = args[0],
            .path = if (args.len > 1) args[1] else ".",
            .limit = 50,
        } };
    } else {
        return .{ .err = try registry_mod.ToolResult.err(allocator, name ++ ": missing query\n") };
    }
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    const parsed_args = switch (try parseArgs(allocator, args)) {
        .err => |e| return e,
        .ok => |p| p,
    };
    defer parsed_args.deinit(allocator);

    const query = parsed_args.query;
    const path = parsed_args.path;
    const limit = parsed_args.limit;

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, path);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    const argv: []const []const u8 = if (std.mem.eql(u8, relative, "."))
        &.{ "rg", "-n", "--no-heading", "--color", "never", "--hidden", "-g", "!.git", "-g", "!zig-out", "-g", "!.zig-cache", "-g", "!node_modules", query }
    else
        &.{ "rg", "-n", "--no-heading", "--color", "never", "--hidden", "-g", "!.git", "-g", "!zig-out", "-g", "!.zig-cache", "-g", "!node_modules", query, relative };

    var outcome = try common.runCommand(allocator, root, argv);
    defer outcome.deinit(allocator);

    const no_matches = outcome.exit_code != null and outcome.exit_code.? == 1 and outcome.stderr.len == 0;
    const semantic_ok = outcome.ok or no_matches;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"ok\":");
    try w.writeAll(if (semantic_ok) "true" else "false");
    try w.writeAll(",\"query\":");
    try json_writer.writeString(w, query);
    try w.writeAll(",\"matches\":[");

    var count: usize = 0;
    var truncated = false;
    var lines = std.mem.splitScalar(u8, outcome.stdout, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (count >= limit) {
            truncated = true;
            break;
        }
        var parts = std.mem.splitScalar(u8, line, ':');
        const file = parts.next() orelse continue;
        const line_str = parts.next() orelse continue;
        const text = parts.rest();
        if (count > 0) try w.writeByte(',');
        try w.writeAll("{\"path\":");
        try json_writer.writeString(w, file);
        try w.writeAll(",\"line\":");
        try w.print("{d}", .{std.fmt.parseInt(usize, line_str, 10) catch 0});
        try w.writeAll(",\"text\":");
        try json_writer.writeString(w, text);
        try w.writeByte('}');
        count += 1;
    }

    try w.writeAll("],\"truncated\":");
    try w.writeAll(if (truncated) "true" else "false");
    try w.writeAll(",\"stderr\":");
    try json_writer.writeString(w, outcome.stderr);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{ .ok = semantic_ok, .llm_text = try buf.toOwnedSlice(allocator) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "workspace_search_text: missing query arg returns structured error" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "missing query") != null);
}

test "workspace_search_text: JSON args without query returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"path\":\".\"}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "missing query") != null);
}

test "workspace_search_text: non-string query returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"query\":42}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "query must be a string") != null);
}

test "workspace_search_text: malformed JSON returns structured error" {
    var result = try execute(testing.allocator, &.{"{not"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "invalid JSON input") != null);
}

test "workspace_search_text: ../ escape is rejected by resolveInsideWorkspace" {
    try testing.expectError(
        error.PathOutsideWorkspace,
        execute(testing.allocator, &.{ "needle", "../../etc" }),
    );
}

// Unwrap a successful parse for tests, freeing the error and failing the test
// on a structured error. The returned ParsedArgs owns its buffers; the caller
// deinits it.
fn parseOk(allocator: std.mem.Allocator, args: []const []const u8) !ParsedArgs {
    var result = try parseArgs(allocator, args);
    switch (result) {
        .err => |*e| {
            e.deinit(allocator);
            return error.UnexpectedParseError;
        },
        .ok => |p| return p,
    }
}

test "workspace_search_text: JSON-derived query survives parse-tree teardown (ae881b6 use-after-free)" {
    // The values must still read correctly after the JSON tree they were parsed
    // from is freed. Before ae881b6 the query was a dangling slice into that
    // freed tree, reaching `rg` as garbage (zero matches). The testing allocator
    // also flags a leak if the owned buffers escape.
    const p = try parseOk(testing.allocator, &.{"{\"query\":\"needle-xyz\",\"path\":\"src\",\"limit\":5}"});
    defer p.deinit(testing.allocator);
    try testing.expectEqualStrings("needle-xyz", p.query);
    try testing.expectEqualStrings("src", p.path);
    try testing.expectEqual(@as(usize, 5), p.limit);
}

test "workspace_search_text: positional args do not allocate owned buffers" {
    const p = try parseOk(testing.allocator, &.{ "needle", "sub/dir" });
    defer p.deinit(testing.allocator);
    try testing.expectEqualStrings("needle", p.query);
    try testing.expectEqualStrings("sub/dir", p.path);
    try testing.expect(p.owned_query == null);
    try testing.expect(p.owned_path == null);
}
