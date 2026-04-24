//! Session persistence primitives: NDJSON events log + meta.json.
//!
//! Event schema `v2` keeps text fields provider-friendly (`llm_text`) while
//! carrying optional structured `ui_payload` objects for replay, RPC, JSON
//! mode, and the TUI.

const std = @import("std");
const zigts = @import("zigts");
const ui_payload = @import("../ui_payload.zig");
const json_writer = @import("../providers/anthropic/json_writer.zig");

pub const schema_version: u32 = 2;

const EventKind = enum {
    user_text,
    model_text,
    tool_use,
    tool_result,
    proof_card,
    diagnostic_box,
    verified_patch,
    system_note,
};

pub const ToolUse = struct {
    id: []const u8,
    name: []const u8,
    args_json: []const u8,
};

pub const DisplayMessage = struct {
    llm_text: []const u8,
    ui_payload: ?ui_payload.UiPayload = null,
};

pub const ToolResult = struct {
    tool_use_id: []const u8,
    tool_name: []const u8,
    ok: bool,
    llm_text: []const u8,
    ui_payload: ?ui_payload.UiPayload = null,
};

pub const EventRecord = union(EventKind) {
    user_text: []const u8,
    model_text: []const u8,
    tool_use: ToolUse,
    tool_result: ToolResult,
    proof_card: DisplayMessage,
    diagnostic_box: DisplayMessage,
    verified_patch: DisplayMessage,
    system_note: []const u8,
};

pub const Meta = struct {
    schema_version: u32 = schema_version,
    session_id: []const u8,
    workspace_realpath: []const u8,
    created_at_unix_ms: i64,
    parent_id: ?[]const u8 = null,
    policy_hash: ?[]const u8 = null,
};

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
    try writer.writeAll("{\"v\":");
    try writer.print("{d}", .{schema_version});
    try writer.writeAll(",\"k\":");
    try json_writer.writeString(writer, kindTag(record));
    try writer.writeAll(",\"d\":");
    try writePayload(writer, record);
    try writer.writeByte('}');
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
        .verified_patch => "verified_patch",
        .system_note => "system_note",
    };
}

fn writePayload(writer: *std.Io.Writer, record: EventRecord) !void {
    switch (record) {
        .user_text, .model_text, .system_note => |body| try json_writer.writeString(writer, body),
        .tool_use => |tu| {
            try writer.writeByte('{');
            try writer.writeAll("\"id\":");
            try json_writer.writeString(writer, tu.id);
            try writer.writeAll(",\"name\":");
            try json_writer.writeString(writer, tu.name);
            try writer.writeAll(",\"args_json\":");
            try json_writer.writeString(writer, tu.args_json);
            try writer.writeByte('}');
        },
        .tool_result => |tr| {
            try writer.writeByte('{');
            try writer.writeAll("\"tool_use_id\":");
            try json_writer.writeString(writer, tr.tool_use_id);
            try writer.writeAll(",\"tool_name\":");
            try json_writer.writeString(writer, tr.tool_name);
            try writer.writeAll(",\"ok\":");
            try writer.writeAll(if (tr.ok) "true" else "false");
            try writer.writeAll(",\"llm_text\":");
            try json_writer.writeString(writer, tr.llm_text);
            try writer.writeAll(",\"body\":");
            try json_writer.writeString(writer, tr.llm_text);
            if (tr.ui_payload) |payload| {
                try writer.writeAll(",\"ui_payload\":");
                try ui_payload.writeJson(writer, payload);
            }
            try writer.writeByte('}');
        },
        .proof_card => |message| try writeDisplayPayload(writer, message),
        .diagnostic_box => |message| try writeDisplayPayload(writer, message),
        .verified_patch => |message| try writeDisplayPayload(writer, message),
    }
}

fn writeDisplayPayload(
    writer: *std.Io.Writer,
    message: DisplayMessage,
) !void {
    try writer.writeByte('{');
    try writer.writeAll("\"llm_text\":");
    try json_writer.writeString(writer, message.llm_text);
    try writer.writeAll(",\"body\":");
    try json_writer.writeString(writer, message.llm_text);
    if (message.ui_payload) |payload| {
        try writer.writeAll(",\"ui_payload\":");
        try ui_payload.writeJson(writer, payload);
    }
    try writer.writeByte('}');
}

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
    if (version_val != .integer or version_val.integer < 0) return error.InvalidMetaJson;
    const version: u32 = std.math.cast(u32, version_val.integer) orelse return error.SchemaVersionTooNew;
    if (version > schema_version) return error.SchemaVersionTooNew;

    const session_id = getRequiredString(obj, "session_id") orelse return error.InvalidMetaJson;
    const workspace_realpath = getRequiredString(obj, "workspace_realpath") orelse return error.InvalidMetaJson;
    const created_at = obj.get("created_at_unix_ms") orelse return error.InvalidMetaJson;
    if (created_at != .integer) return error.InvalidMetaJson;

    const session_id_copy = try allocator.dupe(u8, session_id);
    errdefer allocator.free(session_id_copy);
    const workspace_copy = try allocator.dupe(u8, workspace_realpath);
    errdefer allocator.free(workspace_copy);

    const parent_id = if (getRequiredString(obj, "parent_id")) |pid|
        try allocator.dupe(u8, pid)
    else
        null;
    errdefer if (parent_id) |pid| allocator.free(pid);

    const policy_hash = if (getRequiredString(obj, "policy_hash")) |hash|
        try allocator.dupe(u8, hash)
    else
        null;
    errdefer if (policy_hash) |hash| allocator.free(hash);

    return .{
        .schema_version = version,
        .session_id = session_id_copy,
        .workspace_realpath = workspace_copy,
        .created_at_unix_ms = created_at.integer,
        .parent_id = parent_id,
        .policy_hash = policy_hash,
    };
}

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
    if (meta.parent_id) |parent_id| {
        try stream.objectField("parent_id");
        try stream.write(parent_id);
    }
    if (meta.policy_hash) |hash| {
        try stream.objectField("policy_hash");
        try stream.write(hash);
    }
    try stream.endObject();
    try aw.writer.writeByte('\n');

    buf = aw.toArrayList();
    const tmp_path = try std.fmt.allocPrint(allocator, "{s}.tmp", .{meta_path});
    defer allocator.free(tmp_path);
    try zigts.file_io.writeFile(allocator, tmp_path, buf.items);

    const old_z = try allocator.dupeZ(u8, tmp_path);
    defer allocator.free(old_z);
    const new_z = try allocator.dupeZ(u8, meta_path);
    defer allocator.free(new_z);
    if (std.c.rename(old_z, new_z) != 0) return error.WriteFailure;
}

pub fn freeMeta(allocator: std.mem.Allocator, meta: *Meta) void {
    allocator.free(meta.session_id);
    allocator.free(meta.workspace_realpath);
    if (meta.parent_id) |parent_id| allocator.free(parent_id);
    if (meta.policy_hash) |hash| allocator.free(hash);
    meta.* = .{
        .schema_version = schema_version,
        .session_id = &.{},
        .workspace_realpath = &.{},
        .created_at_unix_ms = 0,
        .parent_id = null,
        .policy_hash = null,
    };
}

fn getRequiredString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    return if (value == .string) value.string else null;
}

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
    try testing.expect(std.mem.indexOf(u8, raw, "\"v\":2") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"k\":\"user_text\"") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"hello world\"") != null);
}

test "appendEvent serializes tool_result with llm_text body alias and ui_payload" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try appendEvent(allocator, path, .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_verify_paths",
        .ok = false,
        .llm_text = "{\"ok\":false}",
        .ui_payload = .{ .plain_text = @constCast("fallback") },
    } });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);
    try testing.expect(std.mem.indexOf(u8, raw, "\"llm_text\":\"{\\\"ok\\\":false}\"") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"body\":\"{\\\"ok\\\":false}\"") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"ui_payload\":{\"kind\":\"plain_text\"") != null);
}

test "appendEvent serializes verified_patch with ui_payload" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    var patch: ui_payload.UiPayload = .{ .verified_patch = .{
        .file = try allocator.dupe(u8, "handler.ts"),
        .policy_hash = try allocator.dupe(u8, "a" ** 64),
        .stats = .{ .total = 0, .new = 0, .preexisting = 0 },
        .before = null,
        .after = try allocator.dupe(u8, "export default {}"),
        .after_properties = null,
        .post_apply_ok = true,
        .post_apply_summary = null,
    } };
    defer patch.deinit(allocator);

    try appendEvent(allocator, path, .{ .verified_patch = .{
        .llm_text = "verified: handler.ts",
        .ui_payload = patch,
    } });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);
    try testing.expect(std.mem.indexOf(u8, raw, "\"k\":\"verified_patch\"") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"ui_payload\":{\"kind\":\"verified_patch\"") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"policy_hash\":\"aaaaa") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"post_apply_ok\":true") != null);
}

test "appendEvent serializes proof_card as a display object" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try appendEvent(allocator, path, .{ .proof_card = .{
        .llm_text = "{\"summary\":{\"total\":0}}",
        .ui_payload = null,
    } });

    const raw = try readWhole(allocator, path);
    defer allocator.free(raw);
    try testing.expect(std.mem.indexOf(u8, raw, "\"k\":\"proof_card\"") != null);
    try testing.expect(std.mem.indexOf(u8, raw, "\"llm_text\":\"{\\\"summary\\\":{\\\"total\\\":0}}\"") != null);
}

test "writeMeta/readMeta round-trip current schema" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "meta.json");
    defer allocator.free(path);

    try writeMeta(allocator, path, .{
        .session_id = "sid",
        .workspace_realpath = "/tmp/ws",
        .created_at_unix_ms = 123,
        .parent_id = "parent",
        .policy_hash = "a" ** 64,
    });

    var meta = try readMeta(allocator, path);
    defer freeMeta(allocator, &meta);

    try testing.expectEqual(@as(u32, schema_version), meta.schema_version);
    try testing.expectEqualStrings("sid", meta.session_id);
    try testing.expectEqualStrings("/tmp/ws", meta.workspace_realpath);
    try testing.expectEqual(@as(i64, 123), meta.created_at_unix_ms);
    try testing.expectEqualStrings("parent", meta.parent_id.?);
}

test "readMeta accepts older schema versions" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "meta.json");
    defer allocator.free(path);

    try zigts.file_io.writeFile(
        allocator,
        path,
        \\{"schema_version":1,"session_id":"sid","workspace_realpath":"/tmp/ws","created_at_unix_ms":123}
        ,
    );

    var meta = try readMeta(allocator, path);
    defer freeMeta(allocator, &meta);
    try testing.expectEqual(@as(u32, 1), meta.schema_version);
}
