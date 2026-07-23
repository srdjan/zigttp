//! Project-scoped, cross-session fact store for the zts expert agent.
//!
//! Storage: `<project_root>/.zttp/memory.jsonl`. One JSONL line per
//! `pi_remember_fact` call. Appends use POSIX `O_APPEND` so two writers
//! whose lines fit in the kernel's PIPE_BUF window (4 KiB on Linux/macOS)
//! cannot tear; longer lines remain safe under a single writer. Mirrors
//! `packages/runtime/src/proof_ledger.zig`'s I/O pattern.
//!
//! Entry schema:
//!   { "id": "hex-or-uuid",
//!     "fact": "string",
//!     "source": "manual_entry|tool|...",
//!     "session_id": "string|null",
//!     "timestamp_unix_ms": 1234567890123,
//!     "pinned": false }
//!
//! `selectForPersona` is the persona-injection helper: it returns pinned
//! entries first, then the most recent unpinned entries, stopping cleanly
//! before the next entry would push the rendered section past
//! `budget_bytes`. Mirrors the shape of slice C's few-shot selector.

const std = @import("std");
const builtin = @import("builtin");
const zts = @import("zts");
const json_utils = zts.json_utils;
const file_io = zts.file_io;

/// Per-project state directory beneath `project_root`. Kept relative so the
/// caller's chosen root drives the location; never a global path.
pub const state_dir_subpath = ".zttp";
pub const file_subpath = ".zttp/memory.jsonl";

/// Cap on how much of `memory.jsonl` we are willing to load into memory.
/// Mirrors `proof_ledger.zig`. Beyond this the file is malformed for our
/// purposes; the alternative is to silently truncate, which would lose
/// pinned context.
const max_file_bytes: usize = 16 * 1024 * 1024;

pub const Source = enum {
    manual_entry,
    tool,
    other,

    pub fn toString(self: Source) []const u8 {
        return switch (self) {
            .manual_entry => "manual_entry",
            .tool => "tool",
            .other => "other",
        };
    }

    pub fn fromString(s: []const u8) Source {
        if (std.mem.eql(u8, s, "manual_entry")) return .manual_entry;
        if (std.mem.eql(u8, s, "tool")) return .tool;
        return .other;
    }
};

/// Owned strings so a parsed entry survives independently of the buffer it
/// came from. `source` is kept as a raw string (not the Source enum) so a
/// future contributor adding a new source on disk does not silently get
/// downgraded to `other` on round-trip.
pub const Entry = struct {
    id: []const u8,
    fact: []const u8,
    source: []const u8,
    session_id: ?[]const u8,
    timestamp_unix_ms: i64,
    pinned: bool,

    pub fn deinit(self: *Entry, allocator: std.mem.Allocator) void {
        allocator.free(self.id);
        allocator.free(self.fact);
        allocator.free(self.source);
        if (self.session_id) |s| allocator.free(s);
        self.* = .{
            .id = &.{},
            .fact = &.{},
            .source = &.{},
            .session_id = null,
            .timestamp_unix_ms = 0,
            .pinned = false,
        };
    }
};

pub const AppendEntry = struct {
    id: []const u8,
    fact: []const u8,
    source: []const u8,
    session_id: ?[]const u8 = null,
    timestamp_unix_ms: i64,
    pinned: bool = false,
};

/// Append a single entry to `<project_root>/.zttp/memory.jsonl`, creating
/// the directory and file if missing. POSIX `O_APPEND` semantics.
pub fn append(
    allocator: std.mem.Allocator,
    project_root: []const u8,
    entry: AppendEntry,
) !void {
    try ensureStateDir(allocator, project_root);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const w = &aw.writer;

    try w.writeAll("{\"id\":");
    try json_utils.writeJsonString(w, entry.id);
    try w.writeAll(",\"fact\":");
    try json_utils.writeJsonString(w, entry.fact);
    try w.writeAll(",\"source\":");
    try json_utils.writeJsonString(w, entry.source);
    try w.writeAll(",\"session_id\":");
    if (entry.session_id) |sid| {
        try json_utils.writeJsonString(w, sid);
    } else {
        try w.writeAll("null");
    }
    try w.print(",\"timestamp_unix_ms\":{d}", .{entry.timestamp_unix_ms});
    try w.writeAll(",\"pinned\":");
    try w.writeAll(if (entry.pinned) "true" else "false");
    try w.writeAll("}\n");

    const bytes = aw.writer.buffered();

    const path = try filePath(allocator, project_root);
    defer allocator.free(path);

    const fd = try file_io.openAppend(allocator, path);
    defer std.Io.Threaded.closeFd(fd);

    var written: usize = 0;
    while (written < bytes.len) {
        const n = std.c.write(fd, bytes[written..].ptr, bytes.len - written);
        if (n < 0) {
            if (std.posix.errno(n) == .INTR) continue;
            return error.MemoryWriteFailed;
        }
        if (n == 0) return error.MemoryWriteFailed;
        written += @intCast(n);
    }
}

/// Read every entry in chronological (file) order. Missing file is not an
/// error: returns an empty slice. Caller frees with `freeEntries`.
pub fn loadAll(
    allocator: std.mem.Allocator,
    project_root: []const u8,
) ![]Entry {
    const path = try filePath(allocator, project_root);
    defer allocator.free(path);

    const bytes = file_io.readFile(allocator, path, max_file_bytes) catch |err| switch (err) {
        error.FileNotFound => return try allocator.alloc(Entry, 0),
        else => return err,
    };
    defer allocator.free(bytes);

    var out: std.ArrayList(Entry) = .empty;
    errdefer {
        for (out.items) |*e| e.deinit(allocator);
        out.deinit(allocator);
    }

    var line_iter = std.mem.splitScalar(u8, bytes, '\n');
    while (line_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;
        const entry = parseLine(allocator, trimmed) catch |err| switch (err) {
            error.InvalidMemoryLine => {
                if (!builtin.is_test) {
                    std.log.err("memory_store: skipping malformed line in {s}", .{path});
                }
                continue;
            },
            else => return err,
        };
        try out.append(allocator, entry);
    }

    return try out.toOwnedSlice(allocator);
}

pub fn freeEntries(allocator: std.mem.Allocator, entries: []Entry) void {
    for (entries) |*e| e.deinit(allocator);
    allocator.free(entries);
}

/// Select the entries the persona should inject this session.
///
/// Ordering: pinned entries first (in original chronological order), then
/// unpinned entries in most-recent-first order. We stop the moment the next
/// candidate's serialized size would push the running total past
/// `budget_bytes`. The "serialized size" we charge against the budget is the
/// length of `entry.fact` plus a constant per-line overhead, so the caller
/// can use `budget_bytes` to bound the rendered section directly.
///
/// Returns a freshly-allocated slice that aliases the input entries' string
/// buffers, so callers must keep the entries returned by `loadAll` alive
/// for as long as the selection is in use. The selection itself is freed
/// with `allocator.free`.
pub fn selectForPersona(
    allocator: std.mem.Allocator,
    entries: []const Entry,
    budget_bytes: usize,
) ![]*const Entry {
    // Two-pass select: pinned (chronological) then unpinned (most recent
    // first). We accumulate into an array list so we can stop cleanly at
    // the budget boundary without pre-counting.
    var out: std.ArrayList(*const Entry) = .empty;
    errdefer out.deinit(allocator);

    var used: usize = 0;

    for (entries) |*e| {
        if (!e.pinned) continue;
        const cost = renderCost(e);
        if (used + cost > budget_bytes) return try out.toOwnedSlice(allocator);
        try out.append(allocator, e);
        used += cost;
    }

    var i: usize = entries.len;
    while (i > 0) {
        i -= 1;
        const e = &entries[i];
        if (e.pinned) continue;
        const cost = renderCost(e);
        if (used + cost > budget_bytes) return try out.toOwnedSlice(allocator);
        try out.append(allocator, e);
        used += cost;
    }

    return try out.toOwnedSlice(allocator);
}

/// Conservative byte cost of rendering a single entry into the PROJECT
/// MEMORY persona section. The persona renderer prints one line per entry
/// of the form `  [pin] fact` plus a trailing newline; this matches that
/// shape with headroom for the marker, the indent, and the newline.
fn renderCost(e: *const Entry) usize {
    // 6 covers two leading spaces, the optional "[pin] " marker (6 chars),
    // and the trailing '\n'. Pinned entries cost the full marker; unpinned
    // entries still get charged the same fixed overhead so the budget can
    // never undershoot.
    return e.fact.len + 8;
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

fn filePath(allocator: std.mem.Allocator, project_root: []const u8) ![]u8 {
    return try std.fs.path.join(allocator, &.{ project_root, file_subpath });
}

fn stateDirPath(allocator: std.mem.Allocator, project_root: []const u8) ![]u8 {
    return try std.fs.path.join(allocator, &.{ project_root, state_dir_subpath });
}

fn ensureStateDir(allocator: std.mem.Allocator, project_root: []const u8) !void {
    const dir = try stateDirPath(allocator, project_root);
    defer allocator.free(dir);
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    try std.Io.Dir.cwd().createDirPath(io_backend.io(), dir);
}

fn parseLine(allocator: std.mem.Allocator, line: []const u8) !Entry {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch
        return error.InvalidMemoryLine;
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidMemoryLine;
    const obj = parsed.value.object;

    const id_v = obj.get("id") orelse return error.InvalidMemoryLine;
    if (id_v != .string) return error.InvalidMemoryLine;
    const fact_v = obj.get("fact") orelse return error.InvalidMemoryLine;
    if (fact_v != .string) return error.InvalidMemoryLine;
    const source_v = obj.get("source") orelse return error.InvalidMemoryLine;
    if (source_v != .string) return error.InvalidMemoryLine;
    const ts_v = obj.get("timestamp_unix_ms") orelse return error.InvalidMemoryLine;
    if (ts_v != .integer) return error.InvalidMemoryLine;
    const pinned_v = obj.get("pinned") orelse return error.InvalidMemoryLine;
    if (pinned_v != .bool) return error.InvalidMemoryLine;

    var session_id: ?[]const u8 = null;
    errdefer if (session_id) |s| allocator.free(s);
    if (obj.get("session_id")) |sv| {
        switch (sv) {
            .null => session_id = null,
            .string => |s| session_id = try allocator.dupe(u8, s),
            else => return error.InvalidMemoryLine,
        }
    }

    const id = try allocator.dupe(u8, id_v.string);
    errdefer allocator.free(id);
    const fact = try allocator.dupe(u8, fact_v.string);
    errdefer allocator.free(fact);
    const source = try allocator.dupe(u8, source_v.string);
    errdefer allocator.free(source);

    return .{
        .id = id,
        .fact = fact,
        .source = source,
        .session_id = session_id,
        .timestamp_unix_ms = ts_v.integer,
        .pinned = pinned_v.bool,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn tmpRoot(tmp: *std.testing.TmpDir, allocator: std.mem.Allocator) ![]u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    return try allocator.dupe(u8, buf[0..len]);
}

test "append + loadAll round-trip on a fresh project root" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmpRoot(&tmp, allocator);
    defer allocator.free(root);

    try append(allocator, root, .{
        .id = "id-001",
        .fact = "use Result<T> for fallible helpers",
        .source = "manual_entry",
        .session_id = null,
        .timestamp_unix_ms = 1_700_000_000_000,
        .pinned = false,
    });
    try append(allocator, root, .{
        .id = "id-002",
        .fact = "session helper lives in session/session_id.zig",
        .source = "tool",
        .session_id = "sess-abc",
        .timestamp_unix_ms = 1_700_000_001_000,
        .pinned = true,
    });

    const entries = try loadAll(allocator, root);
    defer freeEntries(allocator, entries);

    try testing.expectEqual(@as(usize, 2), entries.len);
    try testing.expectEqualStrings("id-001", entries[0].id);
    try testing.expectEqualStrings("use Result<T> for fallible helpers", entries[0].fact);
    try testing.expectEqualStrings("manual_entry", entries[0].source);
    try testing.expect(entries[0].session_id == null);
    try testing.expectEqual(@as(i64, 1_700_000_000_000), entries[0].timestamp_unix_ms);
    try testing.expectEqual(false, entries[0].pinned);

    try testing.expectEqualStrings("id-002", entries[1].id);
    try testing.expect(entries[1].session_id != null);
    try testing.expectEqualStrings("sess-abc", entries[1].session_id.?);
    try testing.expectEqual(true, entries[1].pinned);
}

test "loadAll returns empty when memory.jsonl is missing" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmpRoot(&tmp, allocator);
    defer allocator.free(root);

    const entries = try loadAll(allocator, root);
    defer freeEntries(allocator, entries);
    try testing.expectEqual(@as(usize, 0), entries.len);
}

test "selectForPersona orders pinned first then most-recent unpinned" {
    const allocator = testing.allocator;

    var entries = [_]Entry{
        .{
            .id = try allocator.dupe(u8, "a"),
            .fact = try allocator.dupe(u8, "fact-a"),
            .source = try allocator.dupe(u8, "tool"),
            .session_id = null,
            .timestamp_unix_ms = 1,
            .pinned = false,
        },
        .{
            .id = try allocator.dupe(u8, "b"),
            .fact = try allocator.dupe(u8, "fact-b"),
            .source = try allocator.dupe(u8, "manual_entry"),
            .session_id = null,
            .timestamp_unix_ms = 2,
            .pinned = true,
        },
        .{
            .id = try allocator.dupe(u8, "c"),
            .fact = try allocator.dupe(u8, "fact-c"),
            .source = try allocator.dupe(u8, "tool"),
            .session_id = null,
            .timestamp_unix_ms = 3,
            .pinned = false,
        },
        .{
            .id = try allocator.dupe(u8, "d"),
            .fact = try allocator.dupe(u8, "fact-d"),
            .source = try allocator.dupe(u8, "manual_entry"),
            .session_id = null,
            .timestamp_unix_ms = 4,
            .pinned = true,
        },
    };
    defer for (&entries) |*e| e.deinit(allocator);

    const sel = try selectForPersona(allocator, entries[0..], 4096);
    defer allocator.free(sel);

    try testing.expectEqual(@as(usize, 4), sel.len);
    // Pinned first, in original chronological order (b then d).
    try testing.expectEqualStrings("b", sel[0].id);
    try testing.expectEqualStrings("d", sel[1].id);
    // Then unpinned in most-recent-first order (c then a).
    try testing.expectEqualStrings("c", sel[2].id);
    try testing.expectEqualStrings("a", sel[3].id);
}

test "selectForPersona stops cleanly at the byte budget" {
    const allocator = testing.allocator;

    var entries = [_]Entry{
        .{
            .id = try allocator.dupe(u8, "a"),
            .fact = try allocator.dupe(u8, "AAAAAAAA"), // 8 + 8 overhead = 16
            .source = try allocator.dupe(u8, "manual_entry"),
            .session_id = null,
            .timestamp_unix_ms = 1,
            .pinned = true,
        },
        .{
            .id = try allocator.dupe(u8, "b"),
            .fact = try allocator.dupe(u8, "BBBBBBBB"), // would push to 32
            .source = try allocator.dupe(u8, "manual_entry"),
            .session_id = null,
            .timestamp_unix_ms = 2,
            .pinned = false,
        },
    };
    defer for (&entries) |*e| e.deinit(allocator);

    // Budget = 20 admits the first 16-byte cost but rejects the next.
    const sel = try selectForPersona(allocator, entries[0..], 20);
    defer allocator.free(sel);
    try testing.expectEqual(@as(usize, 1), sel.len);
    try testing.expectEqualStrings("a", sel[0].id);
}

test "selectForPersona on empty input returns an empty slice" {
    const allocator = testing.allocator;
    const sel = try selectForPersona(allocator, &.{}, 1024);
    defer allocator.free(sel);
    try testing.expectEqual(@as(usize, 0), sel.len);
}

test "loadAll round-trips entries with escaped characters in fact body" {
    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmpRoot(&tmp, allocator);
    defer allocator.free(root);

    try append(allocator, root, .{
        .id = "id-1",
        .fact = "line one\nline two\t\"quoted\" \\back",
        .source = "manual_entry",
        .timestamp_unix_ms = 42,
        .pinned = false,
    });

    const entries = try loadAll(allocator, root);
    defer freeEntries(allocator, entries);

    try testing.expectEqual(@as(usize, 1), entries.len);
    try testing.expectEqualStrings("line one\nline two\t\"quoted\" \\back", entries[0].fact);
}
