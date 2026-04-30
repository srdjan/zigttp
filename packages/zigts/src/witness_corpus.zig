//! Witness corpus persistence layer.
//!
//! Compiler-produced counterexample witnesses are valuable evidence: each is
//! a concrete falsifying input that some property at some site can be
//! violated by. Until now they were materialised on demand and discarded.
//! This module persists them to disk so they survive across builds, deploys,
//! live reloads, and developer machines, and so the corpus accumulates as
//! the handler accrues `Spec<...>` declarations.
//!
//! Layout, all paths relative to the project root (cwd of the analyzer
//! invocation):
//!
//!   .zigttp/witnesses/<short_hash>/handler.path      # text, original path
//!   .zigttp/witnesses/<short_hash>/index.jsonl       # append-only event log
//!   .zigttp/witnesses/<short_hash>/<key>.witness.jsonl
//!   .zigttp/witnesses/<short_hash>/<key>.pinned      # marker file (presence = pinned)
//!
//! `<short_hash>` is the first 16 hex chars of sha256(handler_path) to keep
//! corpus directory names short and stable. The original path is recorded
//! once in `handler.path` so `zigts witnesses list` can present it without
//! reversing the hash.
//!
//! `<key>` is `counterexample.CounterexampleWitness.stableKey()` - sha256
//! over (property, origin_node_id|line+col, sink_node_id|line+col). This
//! survives line shifts caused by edits above the witnessing site and means
//! the same logical leak does not get re-persisted as a new entry every
//! time the file is reformatted.
//!
//! Witness files reuse `counterexample.writeJsonl()` exactly so a persisted
//! witness can be replayed via `zigttp mock --replay` without translation.
//!
//! `index.jsonl` records one `created` event per witness on first persist.
//! Re-persisting a known witness is a no-op: no event is appended, so the
//! index does not grow on every build. Pinned state is a separate marker
//! file rather than an event so the filesystem alone tells you what is
//! pinned without log replay.
//!
//! Filesystem access mixes POSIX primitives via `file_io.zig` (read, write,
//! append) with `std.Io.Dir.cwd()` and a locally-created `std.Io.Threaded`
//! for `createDirPath`, `rename`, and `deleteFile` - the same blend
//! `proof_ledger.zig` and `deploy/state.zig` use.

const std = @import("std");
const counterexample = @import("counterexample.zig");
const json_utils = @import("json_utils.zig");
const file_io = @import("file_io.zig");

pub const corpus_root_relative = ".zigttp/witnesses";

/// One entry in the on-disk corpus. All fields are owned by the entry and
/// freed by `freeEntries`.
pub const Entry = struct {
    property: []u8,
    key: []u8,
    summary: []u8,
    first_seen_unix_s: i64,
    pinned: bool,
    file_path: []u8,
};

pub const PersistOutcome = enum { created, refreshed };

pub const PersistResult = struct {
    outcome: PersistOutcome,
    /// Caller-owned 64-char hex stable key.
    key: []u8,
    /// Caller-owned path to the witness file relative to cwd.
    file_path: []u8,

    pub fn deinit(self: *PersistResult, allocator: std.mem.Allocator) void {
        allocator.free(self.key);
        allocator.free(self.file_path);
        self.* = undefined;
    }
};

pub const PropertyCount = struct {
    /// Caller-owned property name.
    property: []u8,
    count: usize,
};

/// Compute the corpus directory for a handler. The directory is not created.
/// Caller owns the returned slice.
pub fn corpusDir(allocator: std.mem.Allocator, handler_path: []const u8) ![]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(handler_path);
    var digest: [32]u8 = undefined;
    hasher.final(&digest);
    const hex = std.fmt.bytesToHex(digest, .lower);
    return std.fmt.allocPrint(
        allocator,
        "{s}/{s}",
        .{ corpus_root_relative, hex[0..16] },
    );
}

/// Create the corpus directory for a handler if it does not exist, and
/// record the original handler path in `handler.path`. Idempotent.
pub fn ensureCorpusDir(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
    handler_path: []const u8,
) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    try std.Io.Dir.cwd().createDirPath(io_backend.io(), corpus_dir);

    const path_file = try std.fmt.allocPrint(
        allocator,
        "{s}/handler.path",
        .{corpus_dir},
    );
    defer allocator.free(path_file);

    if (file_io.fileExists(allocator, path_file)) return;
    try file_io.writeFile(allocator, path_file, handler_path);
}

/// Persist a witness to the corpus. If the witness is already on disk
/// (matched by stable key), the file is left untouched and the index gets a
/// `refreshed` event. Otherwise the witness is written to a temp file and
/// renamed atomically, and the index gets a `created` event.
///
/// Caller owns the returned `PersistResult` strings (call `deinit`).
pub fn persist(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
    witness: counterexample.CounterexampleWitness,
) !PersistResult {
    const key = try keyFor(allocator, witness);
    errdefer allocator.free(key);

    const file_path = try std.fmt.allocPrint(
        allocator,
        "{s}/{s}.witness.jsonl",
        .{ corpus_dir, key },
    );
    errdefer allocator.free(file_path);

    if (file_io.fileExists(allocator, file_path)) {
        // Already on disk - skip both the file write and an index event so
        // index.jsonl does not grow on every build for handlers with a
        // stable witness set.
        return .{
            .outcome = .refreshed,
            .key = key,
            .file_path = file_path,
        };
    }

    const tmp_path = try std.fmt.allocPrint(allocator, "{s}.tmp", .{file_path});
    defer allocator.free(tmp_path);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try counterexample.writeJsonl(&aw.writer, witness);
    try file_io.writeFile(allocator, tmp_path, aw.writer.buffered());

    try renameAtomic(allocator, tmp_path, file_path);

    try appendIndexEvent(allocator, corpus_dir, .{
        .property = witness.property.asString(),
        .key = key,
        .summary = witness.summary,
    });

    return .{
        .outcome = .created,
        .key = key,
        .file_path = file_path,
    };
}

/// Wall-clock seconds since Unix epoch via `clock_gettime(REALTIME)`.
/// Matches the pattern in `proof_ledger.defaultNowMs` but seconds-only.
fn nowUnixSeconds() i64 {
    var ts: std.posix.timespec = undefined;
    switch (std.posix.errno(std.posix.system.clock_gettime(.REALTIME, &ts))) {
        .SUCCESS => return @as(i64, @intCast(ts.sec)),
        else => return 0,
    }
}

fn keyFor(
    allocator: std.mem.Allocator,
    witness: counterexample.CounterexampleWitness,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try witness.stableKey(&aw.writer);
    return allocator.dupe(u8, aw.writer.buffered());
}

fn renameAtomic(
    allocator: std.mem.Allocator,
    old_path: []const u8,
    new_path: []const u8,
) !void {
    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const cwd = std.Io.Dir.cwd();
    try cwd.rename(old_path, cwd, new_path, io_backend.io());
}

const IndexEvent = struct {
    property: []const u8,
    key: []const u8,
    summary: []const u8,
};

fn appendIndexEvent(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
    ev: IndexEvent,
) !void {
    const index_path = try std.fmt.allocPrint(
        allocator,
        "{s}/index.jsonl",
        .{corpus_dir},
    );
    defer allocator.free(index_path);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const w = &aw.writer;

    try w.writeAll("{\"event\":\"created\",\"property\":");
    try json_utils.writeJsonString(w, ev.property);
    try w.writeAll(",\"key\":");
    try json_utils.writeJsonString(w, ev.key);
    try w.writeAll(",\"summary\":");
    try json_utils.writeJsonString(w, ev.summary);
    try w.print(",\"ts\":{d}}}\n", .{nowUnixSeconds()});

    const fd = try file_io.openAppend(allocator, index_path);
    defer std.Io.Threaded.closeFd(fd);
    const bytes = aw.writer.buffered();
    var written: usize = 0;
    while (written < bytes.len) {
        const result = std.c.write(fd, bytes[written..].ptr, bytes.len - written);
        if (result < 0) return error.WitnessCorpusWriteFailed;
        if (result == 0) return error.WitnessCorpusWriteFailed;
        written += @intCast(result);
    }
}

/// Toggle the pinned marker for a witness. Pinned witnesses are protected
/// from `prune`. Idempotent.
pub fn pin(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
    key: []const u8,
    pinned: bool,
) !void {
    const marker = try std.fmt.allocPrint(
        allocator,
        "{s}/{s}.pinned",
        .{ corpus_dir, key },
    );
    defer allocator.free(marker);

    if (pinned) {
        try file_io.writeFile(allocator, marker, "");
    } else {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        std.Io.Dir.cwd().deleteFile(io_backend.io(), marker) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return err,
        };
    }
}

/// Remove unpinned witnesses whose `first_seen_unix_s` is older than the
/// given cutoff. Returns the number of witness files removed. Pinned
/// witnesses are left in place regardless of age.
pub fn prune(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
    older_than_unix_s: i64,
) !usize {
    const entries = loadEntries(allocator, corpus_dir) catch |err| switch (err) {
        error.WitnessCorpusMissing => return 0,
        else => return err,
    };
    defer freeEntries(allocator, entries);

    var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
    defer io_backend.deinit();
    const io = io_backend.io();
    const cwd = std.Io.Dir.cwd();

    var removed: usize = 0;
    for (entries) |e| {
        if (e.pinned) continue;
        if (e.first_seen_unix_s >= older_than_unix_s) continue;
        cwd.deleteFile(io, e.file_path) catch continue;
        removed += 1;
    }
    return removed;
}

/// Hydrate every entry in the corpus for one handler by replaying
/// `index.jsonl` and reconciling against on-disk witness files. Returns
/// `error.WitnessCorpusMissing` if the directory does not exist. Caller
/// frees with `freeEntries`.
pub fn loadEntries(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
) ![]Entry {
    const index_path = try std.fmt.allocPrint(
        allocator,
        "{s}/index.jsonl",
        .{corpus_dir},
    );
    defer allocator.free(index_path);

    if (!file_io.fileExists(allocator, index_path)) {
        // Dir-or-no-index distinction: try to detect a missing dir vs an
        // empty corpus. If `handler.path` is also missing, treat as missing.
        const handler_path_file = try std.fmt.allocPrint(
            allocator,
            "{s}/handler.path",
            .{corpus_dir},
        );
        defer allocator.free(handler_path_file);
        if (!file_io.fileExists(allocator, handler_path_file)) return error.WitnessCorpusMissing;
        return try allocator.alloc(Entry, 0);
    }

    const bytes = file_io.readFile(allocator, index_path, 16 * 1024 * 1024) catch |err| switch (err) {
        error.FileTooBig => return err,
        else => return error.WitnessCorpusMissing,
    };
    defer allocator.free(bytes);

    // Map<key, EntryAccum>: fold events into per-key accumulators. Both
    // the key (dup'd into the map on first insert) and the EntryAccum's
    // owned strings need to be freed on deinit.
    var by_key: std.StringHashMapUnmanaged(EntryAccum) = .empty;
    defer {
        var it = by_key.iterator();
        while (it.next()) |kv| {
            allocator.free(kv.key_ptr.*);
            kv.value_ptr.*.deinit(allocator);
        }
        by_key.deinit(allocator);
    }

    var lines = std.mem.splitScalar(u8, bytes, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch continue;
        defer parsed.deinit();
        if (parsed.value != .object) continue;
        const obj = parsed.value.object;

        const key = obj.get("key") orelse continue;
        if (key != .string) continue;
        const property = obj.get("property") orelse continue;
        if (property != .string) continue;
        const ts = obj.get("ts") orelse continue;
        if (ts != .integer) continue;
        const summary_str = blk: {
            const s = obj.get("summary") orelse break :blk "";
            if (s != .string) break :blk "";
            break :blk s.string;
        };

        const gop = try by_key.getOrPut(allocator, key.string);
        if (gop.found_existing) continue;

        gop.key_ptr.* = try allocator.dupe(u8, key.string);
        gop.value_ptr.* = .{
            .property = try allocator.dupe(u8, property.string),
            .summary = try allocator.dupe(u8, summary_str),
            .first_seen_unix_s = ts.integer,
        };
    }

    var out: std.ArrayList(Entry) = .empty;
    errdefer {
        for (out.items) |*e| freeEntry(allocator, e);
        out.deinit(allocator);
    }

    var it = by_key.iterator();
    while (it.next()) |kv| {
        const key = kv.key_ptr.*;
        const accum = kv.value_ptr.*;

        const file_path = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}.witness.jsonl",
            .{ corpus_dir, key },
        );
        errdefer allocator.free(file_path);

        // Skip entries whose witness file has been pruned manually.
        if (!file_io.fileExists(allocator, file_path)) {
            allocator.free(file_path);
            continue;
        }

        const marker = try std.fmt.allocPrint(
            allocator,
            "{s}/{s}.pinned",
            .{ corpus_dir, key },
        );
        defer allocator.free(marker);

        try out.append(allocator, .{
            .property = try allocator.dupe(u8, accum.property),
            .key = try allocator.dupe(u8, key),
            .summary = try allocator.dupe(u8, accum.summary),
            .first_seen_unix_s = accum.first_seen_unix_s,
            .pinned = file_io.fileExists(allocator, marker),
            .file_path = file_path,
        });
    }

    return out.toOwnedSlice(allocator);
}

const EntryAccum = struct {
    property: []u8,
    summary: []u8,
    first_seen_unix_s: i64,

    fn deinit(self: *EntryAccum, allocator: std.mem.Allocator) void {
        allocator.free(self.property);
        allocator.free(self.summary);
    }
};

pub fn freeEntries(allocator: std.mem.Allocator, entries: []Entry) void {
    for (entries) |*e| freeEntry(allocator, e);
    allocator.free(entries);
}

fn freeEntry(allocator: std.mem.Allocator, e: *Entry) void {
    allocator.free(e.property);
    allocator.free(e.key);
    allocator.free(e.summary);
    allocator.free(e.file_path);
    e.* = undefined;
}

/// Aggregate witness counts per property name. Caller frees with `freeCounts`.
pub fn countByProperty(
    allocator: std.mem.Allocator,
    corpus_dir: []const u8,
) ![]PropertyCount {
    const entries = loadEntries(allocator, corpus_dir) catch |err| switch (err) {
        error.WitnessCorpusMissing => return try allocator.alloc(PropertyCount, 0),
        else => return err,
    };
    defer freeEntries(allocator, entries);

    var counts: std.StringHashMapUnmanaged(usize) = .empty;
    defer counts.deinit(allocator);

    for (entries) |e| {
        const gop = try counts.getOrPut(allocator, e.property);
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    var out = try allocator.alloc(PropertyCount, counts.count());
    errdefer allocator.free(out);

    var i: usize = 0;
    var it = counts.iterator();
    while (it.next()) |kv| : (i += 1) {
        out[i] = .{
            .property = try allocator.dupe(u8, kv.key_ptr.*),
            .count = kv.value_ptr.*,
        };
    }
    return out;
}

pub fn freeCounts(allocator: std.mem.Allocator, counts: []PropertyCount) void {
    for (counts) |c| allocator.free(c.property);
    allocator.free(counts);
}

/// Write the `witnesses` summary block for the proof envelope. Format:
///   {"total":N,"by_property":{"<prop>":N,...}}
pub fn writeProofEnvelopeBlock(
    allocator: std.mem.Allocator,
    writer: anytype,
    corpus_dir: []const u8,
) !void {
    const counts = try countByProperty(allocator, corpus_dir);
    defer freeCounts(allocator, counts);

    var total: usize = 0;
    for (counts) |c| total += c.count;

    try writer.print("{{\"total\":{d},\"by_property\":{{", .{total});
    for (counts, 0..) |c, idx| {
        if (idx > 0) try writer.writeAll(",");
        try json_utils.writeJsonString(writer, c.property);
        try writer.print(":{d}", .{c.count});
    }
    try writer.writeAll("}}");
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

/// Chdir into a fresh tmp dir for the duration of a test. The returned
/// path is the previous cwd; caller restores it on defer.
fn chdirTmp(tmp: *std.testing.TmpDir) ![:0]u8 {
    const old_cwd = try std.process.currentPathAlloc(testing.io, testing.allocator);
    errdefer testing.allocator.free(old_cwd);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const len = try tmp.dir.realPath(testing.io, &buf);
    try std.Io.Threaded.chdir(buf[0..len]);
    return old_cwd;
}

fn makeWitness(allocator: std.mem.Allocator, summary: []const u8) !counterexample.CounterexampleWitness {
    return try counterexample.solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 3, .column = 1 },
        .sink = .{ .line = 7, .column = 12 },
        .origin_node_id = 11,
        .sink_node_id = 22,
        .summary = summary,
        .constraints = &.{},
        .io_calls = &.{},
    });
}

test "corpusDir hashes handler path to a 16-hex short directory" {
    const allocator = testing.allocator;
    const dir = try corpusDir(allocator, "examples/system/users.ts");
    defer allocator.free(dir);
    try testing.expect(std.mem.startsWith(u8, dir, ".zigttp/witnesses/"));
    try testing.expectEqual(@as(usize, ".zigttp/witnesses/".len + 16), dir.len);
}

test "corpusDir is stable across invocations" {
    const allocator = testing.allocator;
    const a = try corpusDir(allocator, "x.ts");
    defer allocator.free(a);
    const b = try corpusDir(allocator, "x.ts");
    defer allocator.free(b);
    try testing.expectEqualStrings(a, b);
}

test "persist creates witness file and reports created outcome" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var witness = try makeWitness(allocator, "secret reaches body");
    defer witness.deinit(allocator);

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var result = try persist(allocator, dir, witness);
    defer result.deinit(allocator);

    try testing.expectEqual(PersistOutcome.created, result.outcome);
    try testing.expect(file_io.fileExists(allocator, result.file_path));
}

test "persist is idempotent on the same witness" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var w = try makeWitness(allocator, "leak");
    defer w.deinit(allocator);

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var first = try persist(allocator, dir, w);
    defer first.deinit(allocator);
    try testing.expectEqual(PersistOutcome.created, first.outcome);

    var second = try persist(allocator, dir, w);
    defer second.deinit(allocator);
    try testing.expectEqual(PersistOutcome.refreshed, second.outcome);
    try testing.expectEqualStrings(first.key, second.key);
}

test "loadEntries hydrates persisted witnesses with summary" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var w = try makeWitness(allocator, "the leak summary");
    defer w.deinit(allocator);

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var r = try persist(allocator, dir, w);
    defer r.deinit(allocator);

    const entries = try loadEntries(allocator, dir);
    defer freeEntries(allocator, entries);

    try testing.expectEqual(@as(usize, 1), entries.len);
    try testing.expectEqualStrings("no_secret_leakage", entries[0].property);
    try testing.expectEqual(@as(usize, 64), entries[0].key.len);
    try testing.expectEqualStrings("the leak summary", entries[0].summary);
    try testing.expect(!entries[0].pinned);
}

test "pin sets and clears the pinned marker" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var w = try makeWitness(allocator, "x");
    defer w.deinit(allocator);

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var r = try persist(allocator, dir, w);
    defer r.deinit(allocator);

    try pin(allocator, dir, r.key, true);

    var entries = try loadEntries(allocator, dir);
    try testing.expect(entries[0].pinned);
    freeEntries(allocator, entries);

    try pin(allocator, dir, r.key, false);
    entries = try loadEntries(allocator, dir);
    defer freeEntries(allocator, entries);
    try testing.expect(!entries[0].pinned);
}

test "pin off is idempotent when marker is already absent" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    var w = try makeWitness(allocator, "x");
    defer w.deinit(allocator);

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var r = try persist(allocator, dir, w);
    defer r.deinit(allocator);

    try pin(allocator, dir, r.key, false);
    try pin(allocator, dir, r.key, false);
}

test "loadEntries returns WitnessCorpusMissing when nothing exists" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try corpusDir(allocator, "ghost.ts");
    defer allocator.free(dir);

    const result = loadEntries(allocator, dir);
    try testing.expectError(error.WitnessCorpusMissing, result);
}

test "countByProperty groups entries by property name" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var w1 = try counterexample.solve(allocator, .{
        .property = .no_secret_leakage,
        .origin = .{ .line = 1, .column = 1 },
        .sink = .{ .line = 2, .column = 1 },
        .origin_node_id = 1,
        .sink_node_id = 2,
        .summary = "a",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer w1.deinit(allocator);
    var r1 = try persist(allocator, dir, w1);
    r1.deinit(allocator);

    var w2 = try counterexample.solve(allocator, .{
        .property = .injection_safe,
        .origin = .{ .line = 5, .column = 1 },
        .sink = .{ .line = 9, .column = 1 },
        .origin_node_id = 3,
        .sink_node_id = 4,
        .summary = "b",
        .constraints = &.{},
        .io_calls = &.{},
    });
    defer w2.deinit(allocator);
    var r2 = try persist(allocator, dir, w2);
    r2.deinit(allocator);

    const counts = try countByProperty(allocator, dir);
    defer freeCounts(allocator, counts);
    try testing.expectEqual(@as(usize, 2), counts.len);

    var total: usize = 0;
    for (counts) |c| total += c.count;
    try testing.expectEqual(@as(usize, 2), total);
}

test "writeProofEnvelopeBlock emits valid JSON shape" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var w = try makeWitness(allocator, "x");
    defer w.deinit(allocator);
    var r = try persist(allocator, dir, w);
    r.deinit(allocator);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try writeProofEnvelopeBlock(allocator, &aw.writer, dir);
    const out = aw.writer.buffered();

    try testing.expect(std.mem.startsWith(u8, out, "{\"total\":1,\"by_property\":{"));
    try testing.expect(std.mem.indexOf(u8, out, "\"no_secret_leakage\":1") != null);
    try testing.expect(std.mem.endsWith(u8, out, "}}"));
}

test "writeProofEnvelopeBlock emits zero-total when corpus is missing" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try writeProofEnvelopeBlock(allocator, &aw.writer, dir);
    try testing.expectEqualStrings("{\"total\":0,\"by_property\":{}}", aw.writer.buffered());
}

test "prune removes only entries older than the cutoff and not pinned" {
    const allocator = testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const old_cwd = try chdirTmp(&tmp);
    defer testing.allocator.free(old_cwd);
    defer std.Io.Threaded.chdir(old_cwd) catch {};

    const dir = try corpusDir(allocator, "h.ts");
    defer allocator.free(dir);
    try ensureCorpusDir(allocator, dir, "h.ts");

    var w = try makeWitness(allocator, "x");
    defer w.deinit(allocator);
    var r = try persist(allocator, dir, w);
    defer r.deinit(allocator);

    // Past cutoff: nothing to remove.
    try testing.expectEqual(@as(usize, 0), try prune(allocator, dir, nowUnixSeconds() - 60));

    const future = nowUnixSeconds() + 60;

    // Pin the only entry; even with an aggressive cutoff it stays.
    try pin(allocator, dir, r.key, true);
    try testing.expectEqual(@as(usize, 0), try prune(allocator, dir, future));

    // Unpin and prune with a future cutoff.
    try pin(allocator, dir, r.key, false);
    try testing.expectEqual(@as(usize, 1), try prune(allocator, dir, future));
}
