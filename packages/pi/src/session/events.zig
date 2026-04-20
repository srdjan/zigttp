//! Session persistence primitives: NDJSON events log + meta.json.
//!
//! Two small, orthogonal writers:
//!
//!   - `appendEvent` appends one JSON line to `events.jsonl`. Line shape:
//!     `{"v":1,"k":"<kind>","d":<payload>}\n`. Short keys keep the log compact;
//!     agentic sessions can produce many events per turn.
//!
//!   - `readMeta` / `writeMeta` round-trip a versioned `meta.json` describing
//!     the session. Writes are atomic (write-to-tmp then rename). Reads reject
//!     any file whose `schema_version` is newer than this binary's constant
//!     with `error.SchemaVersionTooNew` — we are forward-only and never auto-
//!     upgrade across schema boundaries.
//!
//! Wiring into the interactive loop is deferred to Batch 4; this file provides
//! only the low-level serializers and their tests.
//!
//! All file IO uses the POSIX helpers from `zigts.file_io` plus `std.c.rename`
//! for atomic replace, matching the rest of the runtime.

const std = @import("std");
const zigts = @import("zigts");

/// Current schema version understood by this binary. Bump when the on-disk
/// meta.json or events.jsonl shape changes in a breaking way.
pub const schema_version: u32 = 1;

const EventKind = enum {
    user_text,
    model_text,
    tool_use,
    tool_result,
    proof_card,
    diagnostic_box,
};

pub const ToolUse = struct {
    id: []const u8,
    name: []const u8,
    args_json: []const u8,
};

pub const ToolResult = struct {
    tool_use_id: []const u8,
    tool_name: []const u8,
    ok: bool,
    body: []const u8,
};

pub const EventRecord = union(EventKind) {
    user_text: []const u8,
    model_text: []const u8,
    tool_use: ToolUse,
    tool_result: ToolResult,
    proof_card: []const u8,
    diagnostic_box: []const u8,
};

pub const Meta = struct {
    schema_version: u32 = schema_version,
    session_id: []const u8,
    workspace_realpath: []const u8,
    created_at_unix_ms: i64,
};

// ===========================================================================
// appendEvent
// ===========================================================================

/// Append one event as an NDJSON line to `events_path`. Creates the file if
/// it does not exist. Each call writes exactly one line ending in `\n`.
pub fn appendEvent(
    allocator: std.mem.Allocator,
    events_path: []const u8,
    record: EventRecord,
) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    try writeEventLine(&aw.writer, record);

    buf = aw.toArrayList();
    const bytes = buf.items;

    const fd = try zigts.file_io.openAppend(allocator, events_path);
    defer std.Io.Threaded.closeFd(fd);

    var total: usize = 0;
    while (total < bytes.len) {
        const rc = std.c.write(fd, bytes[total..].ptr, bytes.len - total);
        if (rc <= 0) return error.WriteFailure;
        total += @intCast(rc);
    }
}

pub fn writeEventLine(writer: *std.Io.Writer, record: EventRecord) !void {
    var stream: std.json.Stringify = .{ .writer = writer };
    try stream.beginObject();

    try stream.objectField("v");
    try stream.write(schema_version);

    try stream.objectField("k");
    try stream.write(kindTag(record));

    try stream.objectField("d");
    try writePayload(&stream, record);

    try stream.endObject();
    try writer.writeByte('\n');
}

fn kindTag(record: EventRecord) []const u8 {
    return switch (record) {
        .user_text => "user_text",
        .model_text => "model_text",
        .tool_use => "tool_use",
        .tool_result => "tool_result",
        .proof_card => "proof_card",
        .diagnostic_box => "diagnostic_box",
    };
}

fn writePayload(stream: *std.json.Stringify, record: EventRecord) !void {
    switch (record) {
        .user_text, .model_text, .proof_card, .diagnostic_box => |body| {
            try stream.write(body);
        },
        .tool_use => |tu| {
            try stream.beginObject();
            try stream.objectField("id");
            try stream.write(tu.id);
            try stream.objectField("name");
            try stream.write(tu.name);
            try stream.objectField("args_json");
            try stream.write(tu.args_json);
            try stream.endObject();
        },
        .tool_result => |tr| {
            try stream.beginObject();
            try stream.objectField("tool_use_id");
            try stream.write(tr.tool_use_id);
            try stream.objectField("tool_name");
            try stream.write(tr.tool_name);
            try stream.objectField("ok");
            try stream.write(tr.ok);
            try stream.objectField("body");
            try stream.write(tr.body);
            try stream.endObject();
        },
    }
}

// ===========================================================================
// readMeta / writeMeta / freeMeta
// ===========================================================================

/// Read and parse `meta.json`. Rejects any file whose `schema_version` is
/// newer than this binary's constant with `error.SchemaVersionTooNew`.
/// Returned strings are allocator-owned; call `freeMeta` to release them.
pub fn readMeta(allocator: std.mem.Allocator, meta_path: []const u8) !Meta {
    const bytes = try zigts.file_io.readFile(allocator, meta_path, 1 * 1024 * 1024);
    defer allocator.free(bytes);

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, bytes, .{}) catch {
        return error.InvalidMetaJson;
    };
    defer parsed.deinit();

    if (parsed.value != .object) return error.InvalidMetaJson;
    const obj = parsed.value.object;

    const version_val = obj.get("schema_version") orelse return error.InvalidMetaJson;
    if (version_val != .integer) return error.InvalidMetaJson;
    const version_i = version_val.integer;
    if (version_i < 0) return error.InvalidMetaJson;
    const version: u32 = std.math.cast(u32, version_i) orelse return error.SchemaVersionTooNew;
    if (version > schema_version) return error.SchemaVersionTooNew;

    const session_id_val = obj.get("session_id") orelse return error.InvalidMetaJson;
    if (session_id_val != .string) return error.InvalidMetaJson;

    const workspace_val = obj.get("workspace_realpath") orelse return error.InvalidMetaJson;
    if (workspace_val != .string) return error.InvalidMetaJson;

    const created_val = obj.get("created_at_unix_ms") orelse return error.InvalidMetaJson;
    if (created_val != .integer) return error.InvalidMetaJson;

    const session_id = try allocator.dupe(u8, session_id_val.string);
    errdefer allocator.free(session_id);
    const workspace_realpath = try allocator.dupe(u8, workspace_val.string);
    errdefer allocator.free(workspace_realpath);

    return .{
        .schema_version = version,
        .session_id = session_id,
        .workspace_realpath = workspace_realpath,
        .created_at_unix_ms = created_val.integer,
    };
}

/// Serialize `meta` as pretty-printed JSON and write atomically. Always
/// emits this binary's `schema_version`, even if the caller's struct holds
/// a stale value — we never round-trip someone else's version field.
pub fn writeMeta(allocator: std.mem.Allocator, meta_path: []const u8, meta: Meta) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    var stream: std.json.Stringify = .{
        .writer = &aw.writer,
        .options = .{ .whitespace = .indent_2 },
    };
    try stream.beginObject();
    try stream.objectField("schema_version");
    try stream.write(schema_version);
    try stream.objectField("session_id");
    try stream.write(meta.session_id);
    try stream.objectField("workspace_realpath");
    try stream.write(meta.workspace_realpath);
    try stream.objectField("created_at_unix_ms");
    try stream.write(meta.created_at_unix_ms);
    try stream.endObject();
    try aw.writer.writeByte('\n');

    buf = aw.toArrayList();
    const bytes = buf.items;

    const tmp_path = try std.fmt.allocPrint(allocator, "{s}.tmp", .{meta_path});
    defer allocator.free(tmp_path);

    try zigts.file_io.writeFile(allocator, tmp_path, bytes);

    const old_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(old_z);
    const new_z = try allocator.dupeZ(u8, meta_path);
    defer allocator.free(new_z);
    if (std.c.rename(old_z, new_z) != 0) return error.WriteFailure;
}

pub fn freeMeta(allocator: std.mem.Allocator, meta: *Meta) void {
    allocator.free(meta.session_id);
    allocator.free(meta.workspace_realpath);
    meta.* = .{
        .schema_version = schema_version,
        .session_id = &.{},
        .workspace_realpath = &.{},
        .created_at_unix_ms = 0,
    };
}

// ===========================================================================
// Tests
// ===========================================================================

const testing = std.testing;

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
            "zigttp-events-test-{d}-{d}-{d}",
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

test "appendEvent round-trips a user_text event as NDJSON" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try appendEvent(allocator, path, .{ .user_text = "hello world" });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    try testing.expect(raw.len > 0);
    try testing.expectEqual(@as(u8, '\n'), raw[raw.len - 1]);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try testing.expectEqual(@as(i64, @intCast(schema_version)), obj.get("v").?.integer);
    try testing.expectEqualStrings("user_text", obj.get("k").?.string);
    try testing.expectEqualStrings("hello world", obj.get("d").?.string);
}

test "appendEvent separates multiple records with \\n" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try appendEvent(allocator, path, .{ .user_text = "one" });
    try appendEvent(allocator, path, .{ .model_text = "two" });
    try appendEvent(allocator, path, .{ .proof_card = "three" });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var it = std.mem.splitScalar(u8, raw, '\n');
    var lines: std.ArrayList([]const u8) = .empty;
    defer lines.deinit(allocator);
    while (it.next()) |line| {
        if (line.len == 0) continue;
        try lines.append(allocator, line);
    }

    try testing.expectEqual(@as(usize, 3), lines.items.len);
    for (lines.items) |line| {
        var p = try std.json.parseFromSlice(std.json.Value, allocator, line, .{});
        defer p.deinit();
        try testing.expect(p.value == .object);
    }
}

test "appendEvent serializes tool_use with nested id/name/args_json" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try appendEvent(allocator, path, .{ .tool_use = .{
        .id = "toolu_1",
        .name = "zigts_expert_meta",
        .args_json = "{\"verbose\":true}",
    } });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try testing.expectEqualStrings("tool_use", obj.get("k").?.string);

    const d = obj.get("d").?.object;
    try testing.expectEqualStrings("toolu_1", d.get("id").?.string);
    try testing.expectEqualStrings("zigts_expert_meta", d.get("name").?.string);
    try testing.expectEqualStrings("{\"verbose\":true}", d.get("args_json").?.string);
}

test "appendEvent serializes tool_result with ok:bool payload" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try appendEvent(allocator, path, .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .body = "{\"ok\":true}",
    } });
    try appendEvent(allocator, path, .{ .tool_result = .{
        .tool_use_id = "toolu_2",
        .tool_name = "workspace_read_file",
        .ok = false,
        .body = "missing",
    } });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var it = std.mem.splitScalar(u8, raw, '\n');
    const line1 = it.next().?;
    const line2 = it.next().?;

    var p1 = try std.json.parseFromSlice(std.json.Value, allocator, line1, .{});
    defer p1.deinit();
    var p2 = try std.json.parseFromSlice(std.json.Value, allocator, line2, .{});
    defer p2.deinit();

    try testing.expectEqual(true, p1.value.object.get("d").?.object.get("ok").?.bool);
    try testing.expectEqual(false, p2.value.object.get("d").?.object.get("ok").?.bool);
    try testing.expectEqualStrings("toolu_1", p1.value.object.get("d").?.object.get("tool_use_id").?.string);
}

test "writeMeta + readMeta round-trip preserves all fields" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "meta.json");
    defer allocator.free(path);

    const meta_in = Meta{
        .session_id = "sess_abc123",
        .workspace_realpath = "/Users/me/code/proj",
        .created_at_unix_ms = 1_730_000_000_000,
    };
    try writeMeta(allocator, path, meta_in);

    var meta_out = try readMeta(allocator, path);
    defer freeMeta(allocator, &meta_out);

    try testing.expectEqual(@as(u32, schema_version), meta_out.schema_version);
    try testing.expectEqualStrings("sess_abc123", meta_out.session_id);
    try testing.expectEqualStrings("/Users/me/code/proj", meta_out.workspace_realpath);
    try testing.expectEqual(@as(i64, 1_730_000_000_000), meta_out.created_at_unix_ms);
}

test "readMeta rejects files whose schema_version exceeds the binary" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "meta.json");
    defer allocator.free(path);

    const body =
        \\{
        \\  "schema_version": 999,
        \\  "session_id": "sess_future",
        \\  "workspace_realpath": "/tmp/x",
        \\  "created_at_unix_ms": 1
        \\}
    ;
    try zigts.file_io.writeFile(allocator, path, body);

    const result = readMeta(allocator, path);
    try testing.expectError(error.SchemaVersionTooNew, result);
}

test "writeMeta always emits the binary's schema_version, ignoring stale input" {
    const allocator = testing.allocator;
    var tmp = try IsolatedTmp.init(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "meta.json");
    defer allocator.free(path);

    // Caller fabricates a stale version number. Writer must overwrite it.
    try writeMeta(allocator, path, .{
        .schema_version = 0,
        .session_id = "s",
        .workspace_realpath = "/w",
        .created_at_unix_ms = 42,
    });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, raw, .{});
    defer parsed.deinit();

    const on_disk = parsed.value.object.get("schema_version").?.integer;
    try testing.expectEqual(@as(i64, @intCast(schema_version)), on_disk);
}
