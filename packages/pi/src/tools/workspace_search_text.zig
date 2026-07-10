const std = @import("std");
const zigts = @import("zigts");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

const name = "workspace_search_text";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "search text",
    .effect = .execute_process,
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

    // Search via `rg` when present, otherwise fall back to an in-process walk.
    // `runCommand` spawns children under an empty environ and resolves a bare
    // `rg` against PATH; when ripgrep is not installed the exec fails with
    // FileNotFound. Rather than surfacing that as a cryptic tool error (the
    // problem the sibling workspace_list_files tool already fixed), fall back to
    // a zero-dependency in-process substring search over the same files, with
    // the same noise-directory exclusions.
    var output = searchWithRipgrep(allocator, root, relative, query) catch |err| switch (err) {
        error.FileNotFound, error.AccessDenied => try searchInProcess(allocator, root, absolute, query, limit),
        else => return err,
    };
    defer output.deinit(allocator);

    const semantic_ok = output.ok;

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
    var lines = std.mem.splitScalar(u8, output.stdout, '\n');
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
    try json_writer.writeString(w, output.stderr);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{ .ok = semantic_ok, .llm_text = try buf.toOwnedSlice(allocator) };
}

/// Match lines in `rg -n --no-heading` format (`path:line:text`). Both the
/// ripgrep path and the in-process fallback produce this shape so the JSON
/// emitter above stays single-sourced.
const SearchOutput = struct {
    stdout: []u8,
    stderr: []u8,
    ok: bool,

    fn deinit(self: *SearchOutput, allocator: std.mem.Allocator) void {
        allocator.free(self.stdout);
        allocator.free(self.stderr);
    }
};

/// The trailing "--" ends rg's flag parsing, so a model-controlled query
/// beginning with "-" (e.g. "--pre", which executes a command) is always
/// treated as the search pattern, never as an rg option.
const rg_argv_prefix = [_][]const u8{ "rg", "-n", "--no-heading", "--color", "never", "--hidden", "-g", "!.git", "-g", "!zig-out", "-g", "!.zig-cache", "-g", "!node_modules", "--" };

fn buildRgArgv(
    buf: *[rg_argv_prefix.len + 2][]const u8,
    relative: []const u8,
    query: []const u8,
) []const []const u8 {
    @memcpy(buf[0..rg_argv_prefix.len], &rg_argv_prefix);
    buf[rg_argv_prefix.len] = query;
    if (std.mem.eql(u8, relative, ".")) return buf[0 .. rg_argv_prefix.len + 1];
    buf[rg_argv_prefix.len + 1] = relative;
    return buf[0 .. rg_argv_prefix.len + 2];
}

fn searchWithRipgrep(
    allocator: std.mem.Allocator,
    root: []const u8,
    relative: []const u8,
    query: []const u8,
) !SearchOutput {
    var argv_buf: [rg_argv_prefix.len + 2][]const u8 = undefined;
    const argv = buildRgArgv(&argv_buf, relative, query);

    var outcome = try common.runCommand(allocator, root, argv);
    // Capture the verdict fields before deinit clears them, then move the
    // stdout/stderr buffers into SearchOutput so the outcome's own deinit does
    // not free what we are returning.
    // rg exits 1 with no stderr when there are simply no matches; treat that as
    // a successful (empty) search, matching the prior behavior.
    const no_matches = outcome.exit_code != null and outcome.exit_code.? == 1 and outcome.stderr.len == 0;
    const ok = outcome.ok or no_matches;
    const out_stdout = outcome.stdout;
    const out_stderr = outcome.stderr;
    outcome.stdout = &.{};
    outcome.stderr = &.{};
    outcome.deinit(allocator);

    return .{ .stdout = out_stdout, .stderr = out_stderr, .ok = ok };
}

const excluded_names = [_][]const u8{ ".git", "zig-out", ".zig-cache", "node_modules" };

fn isExcluded(entry_name: []const u8) bool {
    for (excluded_names) |ex| {
        if (std.mem.eql(u8, entry_name, ex)) return true;
    }
    return false;
}

/// In-process substring search used when ripgrep is unavailable. Walks the same
/// file set workspace_list_files walks (excluding the noise directories), reads
/// each file, and emits `path:line:text` lines for every line containing the
/// literal `query`. Binary-ish files (those containing a NUL byte) are skipped,
/// mirroring ripgrep's default. Stops once `limit` matches are recorded.
fn searchInProcess(
    allocator: std.mem.Allocator,
    root: []const u8,
    target_abs: []const u8,
    query: []const u8,
    limit: usize,
) !SearchOutput {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var count: usize = 0;
    grepTree(allocator, io, root, target_abs, query, limit, &out, &count, true) catch {
        // A walk error (e.g. unreadable root) yields an empty result set rather
        // than a hard failure; the caller still reports ok=true with no matches.
    };

    return .{
        .stdout = try out.toOwnedSlice(allocator),
        .stderr = try allocator.dupe(u8, ""),
        .ok = true,
    };
}

fn grepTree(
    allocator: std.mem.Allocator,
    io: std.Io,
    root: []const u8,
    dir_abs: []const u8,
    query: []const u8,
    limit: usize,
    out: *std.ArrayList(u8),
    count: *usize,
    is_root: bool,
) !void {
    if (count.* >= limit) return;
    var dir = std.Io.Dir.openDirAbsolute(io, dir_abs, .{ .iterate = true }) catch |err| {
        if (is_root) return err;
        return; // skip an unreadable sub-directory
    };
    defer dir.close(io);

    var it = dir.iterate();
    while (try it.next(io)) |entry| {
        if (count.* >= limit) return;
        if (isExcluded(entry.name)) continue;
        const child_abs = try std.fs.path.join(allocator, &.{ dir_abs, entry.name });
        defer allocator.free(child_abs);
        switch (entry.kind) {
            .directory => try grepTree(allocator, io, root, child_abs, query, limit, out, count, false),
            .file => try grepFile(allocator, root, child_abs, query, limit, out, count),
            else => {},
        }
    }
}

fn grepFile(
    allocator: std.mem.Allocator,
    root: []const u8,
    file_abs: []const u8,
    query: []const u8,
    limit: usize,
    out: *std.ArrayList(u8),
    count: *usize,
) !void {
    // 16 MiB matches the spirit of ripgrep's defaults and keeps a single huge
    // file from exhausting memory; oversized or unreadable files are skipped.
    const contents = zigts.file_io.readFile(allocator, file_abs, 16 * 1024 * 1024) catch return;
    defer allocator.free(contents);
    if (std.mem.indexOfScalar(u8, contents, 0) != null) return; // skip binary files

    const rel = common.relativeToRoot(root, file_abs);
    var line_no: usize = 0;
    var lines = std.mem.splitScalar(u8, contents, '\n');
    while (lines.next()) |line| {
        line_no += 1;
        if (std.mem.indexOf(u8, line, query) == null) continue;
        if (count.* >= limit) return;
        try out.print(allocator, "{s}:{d}:{s}\n", .{ rel, line_no, line });
        count.* += 1;
    }
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

test "workspace_search_text: dash-leading query reaches rg as a pattern, after --" {
    var buf: [rg_argv_prefix.len + 2][]const u8 = undefined;

    const argv_root = buildRgArgv(&buf, ".", "--pre=touch");
    try testing.expectEqualStrings("--", argv_root[argv_root.len - 2]);
    try testing.expectEqualStrings("--pre=touch", argv_root[argv_root.len - 1]);

    const argv_sub = buildRgArgv(&buf, "src", "-e");
    try testing.expectEqualStrings("--", argv_sub[argv_sub.len - 3]);
    try testing.expectEqualStrings("-e", argv_sub[argv_sub.len - 2]);
    try testing.expectEqualStrings("src", argv_sub[argv_sub.len - 1]);
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

const IsolatedTmp = @import("../test_support/tmp.zig").IsolatedTmp;

test "workspace_search_text: in-process fallback finds matches, emits path:line:text, skips noise dirs" {
    // The ripgrep-absent fallback must keep the tool working: it walks the same
    // files workspace_list_files walks, greps each, and emits rg-compatible
    // `path:line:text` lines so the JSON emitter is single-sourced.
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator, "search-inprocess");
    defer tmp.cleanup(allocator);

    try tmp.writeFile(allocator, "src/handler.ts", "const x = 1;\nfind-me here\nbye\n");
    try tmp.writeFile(allocator, "README.md", "nothing\n");
    // A match inside an excluded directory must never surface.
    try tmp.writeFile(allocator, "node_modules/dep.js", "find-me in noise\n");

    var output = try searchInProcess(allocator, tmp.abs_path, tmp.abs_path, "find-me", 50);
    defer output.deinit(allocator);

    try testing.expect(output.ok);
    // The handler hit is on line 2, in rg `-n --no-heading` shape.
    try testing.expect(std.mem.indexOf(u8, output.stdout, "src/handler.ts:2:find-me here") != null);
    // The excluded directory contributed nothing.
    try testing.expect(std.mem.indexOf(u8, output.stdout, "node_modules") == null);
    // The non-matching file contributed nothing.
    try testing.expect(std.mem.indexOf(u8, output.stdout, "README.md") == null);
}

test "workspace_search_text: in-process fallback honors the match limit" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator, "search-inprocess-limit");
    defer tmp.cleanup(allocator);

    try tmp.writeFile(allocator, "a.txt", "needle\nneedle\nneedle\n");

    var output = try searchInProcess(allocator, tmp.abs_path, tmp.abs_path, "needle", 2);
    defer output.deinit(allocator);

    var count: usize = 0;
    var lines = std.mem.splitScalar(u8, output.stdout, '\n');
    while (lines.next()) |line| {
        if (line.len > 0) count += 1;
    }
    try testing.expectEqual(@as(usize, 2), count);
}

test "workspace_search_text: in-process fallback skips binary files" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator, "search-inprocess-binary");
    defer tmp.cleanup(allocator);

    // A NUL byte marks the file as binary; ripgrep skips these by default.
    try tmp.writeFile(allocator, "blob.bin", "find-me\x00more\n");
    try tmp.writeFile(allocator, "text.txt", "find-me\n");

    var output = try searchInProcess(allocator, tmp.abs_path, tmp.abs_path, "find-me", 50);
    defer output.deinit(allocator);

    try testing.expect(std.mem.indexOf(u8, output.stdout, "text.txt:1:find-me") != null);
    try testing.expect(std.mem.indexOf(u8, output.stdout, "blob.bin") == null);
}
