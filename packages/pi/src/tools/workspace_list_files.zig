const std = @import("std");
const registry_mod = @import("../registry/registry.zig");
const common = @import("common.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

const name = "workspace_list_files";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "list files",
    .effect = .read_workspace,
    .description = "List workspace files relative to the repo root.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"limit\":{\"type\":\"integer\",\"minimum\":1}},\"required\":[]}",
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    var path: []const u8 = ".";
    var limit: usize = 200;
    // A JSON-derived path must outlive `parsed.deinit()` (it is used below for
    // resolveInsideWorkspace); the raw `args` slices are caller-owned and outlive
    // this call, so only the parsed value is duped.
    var owned_path: ?[]u8 = null;
    defer if (owned_path) |p| allocator.free(p);

    if (args.len > 0 and args[0].len > 0 and args[0][0] == '{') {
        var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
            return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
        };
        defer parsed.deinit();
        if (parsed.value != .object) return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
        const obj = parsed.value.object;
        if (obj.get("path")) |value| {
            if (value != .string) return registry_mod.ToolResult.err(allocator, name ++ ": path must be a string\n");
            owned_path = try allocator.dupe(u8, value.string);
            path = owned_path.?;
        }
        if (obj.get("limit")) |value| {
            if (value != .integer or value.integer <= 0) return registry_mod.ToolResult.err(allocator, name ++ ": limit must be a positive integer\n");
            limit = @intCast(value.integer);
        }
    } else if (args.len > 0) {
        path = args[0];
    }

    const root = try common.workspaceRoot(allocator);
    defer allocator.free(root);
    const absolute = try common.resolveInsideWorkspace(allocator, root, path);
    defer allocator.free(absolute);
    const relative = common.relativeToRoot(root, absolute);

    // List files with an in-process recursive walk rather than shelling out to
    // `rg --files`. Subprocesses run under an empty environment (no PATH) for
    // sandbox isolation, so a bare `rg` exec fails with FileNotFound and the
    // agent never learns the workspace layout. An in-process walk needs no
    // external binary, matches the project's zero-dependency stance, and is
    // deterministic. It excludes the same noise directories the old
    // `rg -g '!...'` globs did.
    var files = std.ArrayList([]const u8).empty;
    defer {
        for (files.items) |f| allocator.free(f);
        files.deinit(allocator);
    }
    var truncated = false;
    var ok = true;
    var err_name: []const u8 = "";

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    collectFiles(allocator, io_backend.io(), root, absolute, limit, &files, &truncated, true) catch |err| {
        ok = false;
        err_name = @errorName(err);
    };

    // Deterministic ordering: a filesystem walk visits entries in OS-dependent
    // order, so sort before emitting.
    std.mem.sort([]const u8, files.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":");
    try w.writeAll(if (ok) "true" else "false");
    try w.writeAll(",\"path\":");
    try json_writer.writeString(w, relative);
    try w.writeAll(",\"truncated\":");
    try w.writeAll(if (truncated) "true" else "false");
    try w.writeAll(",\"files\":[");
    for (files.items, 0..) |file, i| {
        if (i > 0) try w.writeByte(',');
        try json_writer.writeString(w, file);
    }
    try w.writeAll("],\"stderr\":");
    try json_writer.writeString(w, err_name);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{ .ok = ok, .llm_text = try buf.toOwnedSlice(allocator) };
}

const excluded_names = [_][]const u8{ ".git", "zig-out", ".zig-cache", "node_modules" };

fn isExcluded(entry_name: []const u8) bool {
    for (excluded_names) |ex| {
        if (std.mem.eql(u8, entry_name, ex)) return true;
    }
    return false;
}

/// Recursively collect file paths under `dir_abs`, recorded relative to `root`,
/// skipping the excluded noise directories. An unopenable *root* is a hard
/// error (`is_root`); unreadable sub-directories are skipped so a single
/// permission hiccup does not abort the whole listing. Only `.directory`
/// entries are recursed into, so a symlink cycle cannot loop the walk.
fn collectFiles(
    allocator: std.mem.Allocator,
    io: std.Io,
    root: []const u8,
    dir_abs: []const u8,
    limit: usize,
    files: *std.ArrayList([]const u8),
    truncated: *bool,
    is_root: bool,
) !void {
    if (truncated.*) return;
    var dir = std.Io.Dir.openDirAbsolute(io, dir_abs, .{ .iterate = true }) catch |err| {
        if (is_root) return err;
        return; // skip an unreadable sub-directory
    };
    defer dir.close(io);

    var it = dir.iterate();
    while (try it.next(io)) |entry| {
        if (isExcluded(entry.name)) continue;
        const child_abs = try std.fs.path.join(allocator, &.{ dir_abs, entry.name });
        defer allocator.free(child_abs);
        switch (entry.kind) {
            .directory => try collectFiles(allocator, io, root, child_abs, limit, files, truncated, false),
            .file, .sym_link => {
                if (files.items.len >= limit) {
                    truncated.* = true;
                    return;
                }
                const rel = common.relativeToRoot(root, child_abs);
                try files.append(allocator, try allocator.dupe(u8, rel));
            },
            else => {},
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "workspace_list_files: malformed JSON args returns structured error" {
    var result = try execute(testing.allocator, &.{"{not json"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "invalid JSON input") != null);
}

test "workspace_list_files: non-string path returns structured error" {
    var result = try execute(testing.allocator, &.{"{\"path\":123}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "path must be a string") != null);
}

test "workspace_list_files: limit must be a positive integer" {
    var result = try execute(testing.allocator, &.{"{\"limit\":0}"});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "limit must be a positive integer") != null);
}

test "workspace_list_files: ../ escape is rejected by resolveInsideWorkspace" {
    try testing.expectError(
        error.PathOutsideWorkspace,
        execute(testing.allocator, &.{"../../etc"}),
    );
}

test "collectFiles: in-process walk lists files relative to root and skips noise dirs" {
    const allocator = testing.allocator;
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(io, "src");
    try tmp.dir.createDirPath(io, ".git");
    try tmp.dir.createDirPath(io, "zig-out");
    try tmp.dir.createDirPath(io, "node_modules");
    inline for (.{ "src/handler.ts", "README.md", ".git/config", "zig-out/bin", "node_modules/dep.js" }) |p| {
        var f = try tmp.dir.createFile(io, p, .{});
        f.close(io);
    }

    var pathbuf: [std.fs.max_path_bytes]u8 = undefined;
    const root_len = try tmp.dir.realPath(io, &pathbuf);
    const root = pathbuf[0..root_len];

    var files = std.ArrayList([]const u8).empty;
    defer {
        for (files.items) |f| allocator.free(f);
        files.deinit(allocator);
    }
    var truncated = false;
    try collectFiles(allocator, io, root, root, 200, &files, &truncated, true);

    var saw_handler = false;
    var saw_readme = false;
    for (files.items) |f| {
        if (std.mem.eql(u8, f, "src/handler.ts")) saw_handler = true;
        if (std.mem.eql(u8, f, "README.md")) saw_readme = true;
        // Nothing from an excluded directory may appear.
        try testing.expect(std.mem.indexOf(u8, f, ".git") == null);
        try testing.expect(std.mem.indexOf(u8, f, "zig-out") == null);
        try testing.expect(std.mem.indexOf(u8, f, "node_modules") == null);
    }
    try testing.expect(saw_handler);
    try testing.expect(saw_readme);
    try testing.expect(!truncated);
}

test "collectFiles: honors the limit and marks truncated" {
    const allocator = testing.allocator;
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    inline for (.{ "a.ts", "b.ts", "c.ts" }) |p| {
        var f = try tmp.dir.createFile(io, p, .{});
        f.close(io);
    }

    var pathbuf: [std.fs.max_path_bytes]u8 = undefined;
    const root_len = try tmp.dir.realPath(io, &pathbuf);
    const root = pathbuf[0..root_len];

    var files = std.ArrayList([]const u8).empty;
    defer {
        for (files.items) |f| allocator.free(f);
        files.deinit(allocator);
    }
    var truncated = false;
    try collectFiles(allocator, io, root, root, 2, &files, &truncated, true);

    try testing.expectEqual(@as(usize, 2), files.items.len);
    try testing.expect(truncated);
}
