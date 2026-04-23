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
    system_note,
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
    system_note: []const u8,
};

pub const Meta = struct {
    schema_version: u32 = schema_version,
    session_id: []const u8,
    workspace_realpath: []const u8,
    created_at_unix_ms: i64,
    parent_id: ?[]const u8 = null,
    /// 64-char lowercase hex policy hash captured at session creation. Drives
    /// the /resume drift warning: a hash mismatch on resume means the rule
    /// registry changed between session init and the replay, and the old
    /// transcript reasoning may be stale against today's compiler policy.
    /// Optional so pre-Phase-2 `meta.json` files still parse.
    policy_hash: ?[]const u8 = null,
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
        .system_note => "system_note",
    };
}

fn writePayload(stream: *std.json.Stringify, record: EventRecord) !void {
    switch (record) {
        .user_text, .model_text, .proof_card, .diagnostic_box, .system_note => |body| {
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

    var parent_id: ?[]u8 = null;
    if (obj.get("parent_id")) |pid_val| {
        if (pid_val == .string) {
            parent_id = try allocator.dupe(u8, pid_val.string);
        }
    }
    errdefer if (parent_id) |p| allocator.free(p);

    var policy_hash: ?[]u8 = null;
    if (obj.get("policy_hash")) |ph_val| {
        if (ph_val == .string) {
            policy_hash = try allocator.dupe(u8, ph_val.string);
        }
    }
    errdefer if (policy_hash) |p| allocator.free(p);

    return .{
        .schema_version = version,
        .session_id = session_id,
        .workspace_realpath = workspace_realpath,
        .created_at_unix_ms = created_val.integer,
        .parent_id = parent_id,
        .policy_hash = policy_hash,
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
    if (meta.parent_id) |pid| {
        try stream.objectField("parent_id");
        try stream.write(pid);
    }
    if (meta.policy_hash) |ph| {
        try stream.objectField("policy_hash");
        try stream.write(ph);
    }
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
    if (meta.parent_id) |p| allocator.free(p);
    if (meta.policy_hash) |p| allocator.free(p);
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

const IsolatedTmp = @import("../test_support/tmp.zig").IsolatedTmp;

fn initTmp(allocator: std.mem.Allocator) !IsolatedTmp {
    return IsolatedTmp.init(allocator, "events");
}

fn readWhole(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    return try zigts.file_io.readFile(allocator, path, 1 * 1024 * 1024);
}

test "appendEvent round-trips a user_text event as NDJSON" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
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
    var tmp = try initTmp(allocator);
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
    var tmp = try initTmp(allocator);
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
    var tmp = try initTmp(allocator);
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
    var tmp = try initTmp(allocator);
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

test "writeMeta + readMeta round-trip preserves policy_hash" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "meta.json");
    defer allocator.free(path);

    const hash = "a" ** 64; // lowercase hex sentinel, 64 chars
    try writeMeta(allocator, path, .{
        .session_id = "sess_hash",
        .workspace_realpath = "/w",
        .created_at_unix_ms = 1,
        .policy_hash = hash,
    });

    var meta_out = try readMeta(allocator, path);
    defer freeMeta(allocator, &meta_out);

    const got = meta_out.policy_hash orelse return error.TestExpected;
    try testing.expectEqualStrings(hash, got);
}

test "readMeta accepts legacy meta.json without policy_hash" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "meta.json");
    defer allocator.free(path);

    const body =
        \\{
        \\  "schema_version": 1,
        \\  "session_id": "legacy",
        \\  "workspace_realpath": "/w",
        \\  "created_at_unix_ms": 1
        \\}
    ;
    try zigts.file_io.writeFile(allocator, path, body);

    var meta_out = try readMeta(allocator, path);
    defer freeMeta(allocator, &meta_out);
    try testing.expect(meta_out.policy_hash == null);
}

test "readMeta rejects files whose schema_version exceeds the binary" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
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
    var tmp = try initTmp(allocator);
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
