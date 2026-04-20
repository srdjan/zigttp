//! Pure, wire-free persistence helper that maps `transcript.OwnedEntry`
//! values to `session/events.EventRecord` values and appends them to an
//! `events.jsonl` file.
//!
//! Two small, orthogonal helpers:
//!
//!   - `entryToEvent` is a pure mapping from one transcript variant to its
//!     matching event variant. It borrows slices from the input entry and
//!     does no allocation. For `.assistant_tool_use`, which carries N
//!     `OwnedToolCall`s, this helper returns only the first call; callers
//!     that need all of them should use `appendEntry`, which iterates
//!     internally and emits one event per call.
//!
//!   - `appendEntry` writes the entry to disk honoring `AppendOptions`:
//!       * `no_persist_tool_output = true` skips `.tool_result` entries
//!         entirely. All other variants still persist.
//!       * `max_body` caps oversized `.tool_result` bodies via
//!         `transcript.capToolResultBody` before writing. The input entry
//!         is never mutated.
//!
//! This unit does not touch the loop, agent, transcript, or TUI; it is
//! a building block for later wiring.

const std = @import("std");
const transcript = @import("../transcript.zig");
const events = @import("events.zig");

pub const AppendOptions = struct {
    no_persist_tool_output: bool = false,
    max_body: usize = 256 * 1024,
};

/// Pure mapping from a transcript variant to the corresponding event
/// variant. Borrows slices from `entry`; do not persist the returned
/// record past `entry`'s lifetime unless `events.appendEvent` has already
/// been called with it.
///
/// NOTE: `.assistant_tool_use` carries a slice of `OwnedToolCall`. This
/// helper maps only the first call. For the multi-call case, use
/// `appendEntry`, which emits exactly one event per `ToolCall` in the
/// slice — matching the "every tool_use has a matching tool_result"
/// invariant.
fn entryToEvent(entry: *const transcript.OwnedEntry) events.EventRecord {
    return switch (entry.*) {
        .user_text => |body| .{ .user_text = body },
        .model_text => |body| .{ .model_text = body },
        .proof_card => |body| .{ .proof_card = body },
        .diagnostic_box => |body| .{ .diagnostic_box = body },
        .assistant_tool_use => |calls| .{ .tool_use = .{
            .id = calls[0].id,
            .name = calls[0].name,
            .args_json = calls[0].args_json,
        } },
        .tool_result => |tr| .{ .tool_result = .{
            .tool_use_id = tr.tool_use_id,
            .tool_name = tr.tool_name,
            .ok = tr.ok,
            .body = tr.body,
        } },
    };
}

/// Append one or more `events.jsonl` lines for `entry`, honoring `opts`.
///
/// `.assistant_tool_use` with N calls emits N lines (one `tool_use` event
/// per call). All other variants emit exactly one line.
///
/// `.tool_result` is skipped entirely when `opts.no_persist_tool_output`
/// is true. When present and `body.len > opts.max_body`, the body is
/// capped via `transcript.capToolResultBody` before writing; the input
/// entry is never mutated.
pub fn appendEntry(
    allocator: std.mem.Allocator,
    events_path: []const u8,
    entry: *const transcript.OwnedEntry,
    opts: AppendOptions,
) !void {
    switch (entry.*) {
        .tool_result => |tr| {
            if (opts.no_persist_tool_output) return;
            const over_cap = tr.body.len > opts.max_body;
            const body = if (over_cap)
                try transcript.capToolResultBody(allocator, tr.body, opts.max_body)
            else
                tr.body;
            defer if (over_cap) allocator.free(body);
            try events.appendEvent(allocator, events_path, .{ .tool_result = .{
                .tool_use_id = tr.tool_use_id,
                .tool_name = tr.tool_name,
                .ok = tr.ok,
                .body = body,
            } });
        },
        .assistant_tool_use => |calls| {
            for (calls) |call| {
                try events.appendEvent(allocator, events_path, .{ .tool_use = .{
                    .id = call.id,
                    .name = call.name,
                    .args_json = call.args_json,
                } });
            }
        },
        else => try events.appendEvent(allocator, events_path, entryToEvent(entry)),
    }
}

// ===========================================================================
// Tests
// ===========================================================================

const testing = std.testing;
const zigts = @import("zigts");

var isolated_tmp_counter = std.atomic.Value(u64).init(0);

const IsolatedTmp = struct {
    abs_path: []u8,
    name: []u8,

    fn init(allocator: std.mem.Allocator) !IsolatedTmp {
        var ts: std.posix.timespec = undefined;
        _ = std.c.clock_gettime(@enumFromInt(@intFromEnum(std.posix.CLOCK.REALTIME)), &ts);
        const counter = isolated_tmp_counter.fetchAdd(1, .seq_cst);
        const name = try std.fmt.allocPrint(
            allocator,
            "zigttp-persister-test-{d}-{d}-{d}",
            .{ @as(u64, @intCast(ts.sec)), @as(u64, @intCast(ts.nsec)), counter },
        );
        errdefer allocator.free(name);

        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        const io = io_backend.io();

        var tmp_root = try std.Io.Dir.openDirAbsolute(io, "/tmp", .{});
        defer tmp_root.close(io);
        tmp_root.deleteTree(io, name) catch {};
        try std.Io.Dir.createDirPath(tmp_root, io, name);

        const abs_path = try std.fs.path.resolve(allocator, &.{ "/tmp", name });
        errdefer allocator.free(abs_path);

        return .{ .abs_path = abs_path, .name = name };
    }

    fn cleanup(self: *IsolatedTmp, allocator: std.mem.Allocator) void {
        var io_backend = std.Io.Threaded.init(allocator, .{ .environ = .empty });
        defer io_backend.deinit();
        const io = io_backend.io();
        var tmp_root = std.Io.Dir.openDirAbsolute(io, "/tmp", .{}) catch {
            allocator.free(self.abs_path);
            allocator.free(self.name);
            return;
        };
        defer tmp_root.close(io);
        tmp_root.deleteTree(io, self.name) catch {};
        allocator.free(self.abs_path);
        allocator.free(self.name);
    }

    fn childPath(self: *const IsolatedTmp, allocator: std.mem.Allocator, name: []const u8) ![]u8 {
        return try std.fs.path.resolve(allocator, &.{ self.abs_path, name });
    }
};

fn readWhole(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    return try zigts.file_io.readFile(allocator, path, 1 * 1024 * 1024);
}

fn splitLines(allocator: std.mem.Allocator, raw: []const u8) !std.ArrayList([]const u8) {
    var lines: std.ArrayList([]const u8) = .empty;
    errdefer lines.deinit(allocator);
    var it = std.mem.splitScalar(u8, raw, '\n');
    while (it.next()) |line| {
        if (line.len == 0) continue;
        try lines.append(allocator, line);
    }
    return lines;
}

test "appendEntry persists a user_text entry" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    const entry: transcript.OwnedEntry = .{ .user_text = "hello user" };
    try appendEntry(allocator, path, &entry, .{});

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try testing.expectEqualStrings("user_text", obj.get("k").?.string);
    try testing.expectEqualStrings("hello user", obj.get("d").?.string);
}

test "appendEntry persists a model_text entry" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    const entry: transcript.OwnedEntry = .{ .model_text = "Thinking..." };
    try appendEntry(allocator, path, &entry, .{});

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try testing.expectEqualStrings("model_text", obj.get("k").?.string);
    try testing.expectEqualStrings("Thinking...", obj.get("d").?.string);
}

test "appendEntry on assistant_tool_use with 2 calls writes 2 distinct tool_use lines" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    var calls = [_]transcript.OwnedToolCall{
        .{ .id = "toolu_a", .name = "zigts_expert_meta", .args_json = "{}" },
        .{ .id = "toolu_b", .name = "workspace_read_file", .args_json = "{\"path\":\"x.ts\"}" },
    };
    const entry: transcript.OwnedEntry = .{ .assistant_tool_use = calls[0..] };
    try appendEntry(allocator, path, &entry, .{});

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var lines = try splitLines(allocator, raw);
    defer lines.deinit(allocator);
    try testing.expectEqual(@as(usize, 2), lines.items.len);

    var p1 = try std.json.parseFromSlice(std.json.Value, allocator, lines.items[0], .{});
    defer p1.deinit();
    var p2 = try std.json.parseFromSlice(std.json.Value, allocator, lines.items[1], .{});
    defer p2.deinit();

    try testing.expectEqualStrings("tool_use", p1.value.object.get("k").?.string);
    try testing.expectEqualStrings("tool_use", p2.value.object.get("k").?.string);

    const d1 = p1.value.object.get("d").?.object;
    const d2 = p2.value.object.get("d").?.object;
    try testing.expectEqualStrings("toolu_a", d1.get("id").?.string);
    try testing.expectEqualStrings("zigts_expert_meta", d1.get("name").?.string);
    try testing.expectEqualStrings("toolu_b", d2.get("id").?.string);
    try testing.expectEqualStrings("workspace_read_file", d2.get("name").?.string);
}

test "appendEntry on tool_result under cap writes body unchanged" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    const entry: transcript.OwnedEntry = .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .body = "{\"ok\":true}",
    } };
    try appendEntry(allocator, path, &entry, .{ .max_body = 1024 });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const d = parsed.value.object.get("d").?.object;
    try testing.expectEqualStrings("{\"ok\":true}", d.get("body").?.string);
    try testing.expectEqual(true, d.get("ok").?.bool);
}

test "appendEntry on tool_result over cap writes a truncated body with dropped byte count" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    var big: [1000]u8 = undefined;
    @memset(&big, 'a');

    const entry: transcript.OwnedEntry = .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "workspace_read_file",
        .ok = true,
        .body = &big,
    } };
    try appendEntry(allocator, path, &entry, .{ .max_body = 100 });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const body = parsed.value.object.get("d").?.object.get("body").?.string;
    try testing.expect(body.len <= 100);
    try testing.expect(std.mem.indexOf(u8, body, "[truncated") != null);

    // Verify the suffix encodes the exact number of dropped bytes.
    const suffix_prefix = "\n...[truncated ";
    const idx = std.mem.indexOf(u8, body, suffix_prefix) orelse return error.TestFailed;
    const digits_start = idx + suffix_prefix.len;
    const digits_end = std.mem.indexOfScalarPos(u8, body, digits_start, ' ') orelse return error.TestFailed;
    const dropped = try std.fmt.parseInt(usize, body[digits_start..digits_end], 10);
    try testing.expectEqual(big.len - idx, dropped);

    // Input entry must not have been mutated.
    switch (entry) {
        .tool_result => |tr| try testing.expectEqual(@as(usize, 1000), tr.body.len),
        else => return error.TestFailed,
    }
}

test "appendEntry with no_persist_tool_output skips tool_result entries" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    const entry: transcript.OwnedEntry = .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "workspace_read_file",
        .ok = true,
        .body = "secret output",
    } };
    try appendEntry(allocator, path, &entry, .{ .no_persist_tool_output = true });

    // File must not have been created by the skipped write.
    try testing.expect(!zigts.file_io.fileExists(allocator, path));
}

test "appendEntry with no_persist_tool_output still persists user_text" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    const entry: transcript.OwnedEntry = .{ .user_text = "still persisted" };
    try appendEntry(allocator, path, &entry, .{ .no_persist_tool_output = true });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try testing.expectEqualStrings("user_text", obj.get("k").?.string);
    try testing.expectEqualStrings("still persisted", obj.get("d").?.string);
}

test "appendEntry on proof_card and diagnostic_box round-trip correctly" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    const proof: transcript.OwnedEntry = .{ .proof_card = "contract ok" };
    const diag: transcript.OwnedEntry = .{ .diagnostic_box = "ZTS001 veto" };
    try appendEntry(allocator, path, &proof, .{});
    try appendEntry(allocator, path, &diag, .{});

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var lines = try splitLines(allocator, raw);
    defer lines.deinit(allocator);
    try testing.expectEqual(@as(usize, 2), lines.items.len);

    var p1 = try std.json.parseFromSlice(std.json.Value, allocator, lines.items[0], .{});
    defer p1.deinit();
    var p2 = try std.json.parseFromSlice(std.json.Value, allocator, lines.items[1], .{});
    defer p2.deinit();

    try testing.expectEqualStrings("proof_card", p1.value.object.get("k").?.string);
    try testing.expectEqualStrings("contract ok", p1.value.object.get("d").?.string);
    try testing.expectEqualStrings("diagnostic_box", p2.value.object.get("k").?.string);
    try testing.expectEqualStrings("ZTS001 veto", p2.value.object.get("d").?.string);
}
