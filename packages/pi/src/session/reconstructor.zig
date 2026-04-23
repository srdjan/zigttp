//! Rebuild an owned `Transcript` from an `events.jsonl` NDJSON log.
//!
//! Pure, wire-free helper used by resume flows to restore a previous session
//! into memory. Each `k="tool_use"` event becomes one single-call
//! `.assistant_tool_use` entry (one event -> one entry, matching the
//! persister's one-tool_use-per-ToolCall emission shape). No coalescing.
//!
//! Errors:
//!   - `error.SchemaVersionTooNew` — any line's `v` exceeds the binary's
//!     `events.schema_version`.
//!   - `error.CorruptEventsLog` — JSON parse failure, missing required
//!     fields, wrong field types, or unknown `k`. When `diag` is non-null,
//!     `line_number` (1-based) and a short message are populated.
//!   - Underlying file-IO errors propagate (`error.FileNotFound`, etc.).
//!
//! Empty file returns a fresh empty `Transcript`.

const std = @import("std");
const zigts = @import("zigts");

const transcript = @import("../transcript.zig");
const events = @import("events.zig");

pub const Diagnostic = struct {
    line_number: usize = 0,
    message: []const u8 = "",
};

/// Reads the NDJSON events log at `events_path` and returns an owned
/// `Transcript`. Caller frees via `transcript.deinit(allocator)`.
pub fn reconstructTranscript(
    allocator: std.mem.Allocator,
    events_path: []const u8,
    diag: ?*Diagnostic,
) !transcript.Transcript {
    const raw = try zigts.file_io.readFile(allocator, events_path, 64 * 1024 * 1024);
    defer allocator.free(raw);

    var tr: transcript.Transcript = .{};
    errdefer tr.deinit(allocator);

    if (raw.len == 0) return tr;

    var line_number: usize = 0;
    var it = std.mem.splitScalar(u8, raw, '\n');
    while (it.next()) |line| {
        if (line.len == 0) continue;
        line_number += 1;
        appendFromLine(allocator, &tr, line) catch |err| switch (err) {
            error.SchemaVersionTooNew => {
                return error.SchemaVersionTooNew;
            },
            error.CorruptEventsLog => {
                if (diag) |d| d.* = .{
                    .line_number = line_number,
                    .message = "invalid or malformed events.jsonl line",
                };
                return error.CorruptEventsLog;
            },
            else => |e| return e,
        };
    }

    return tr;
}

fn appendFromLine(
    allocator: std.mem.Allocator,
    tr: *transcript.Transcript,
    line: []const u8,
) !void {
    var parsed = std.json.parseFromSlice(std.json.Value, allocator, line, .{}) catch {
        return error.CorruptEventsLog;
    };
    defer parsed.deinit();

    if (parsed.value != .object) return error.CorruptEventsLog;
    const obj = parsed.value.object;

    const version_val = obj.get("v") orelse return error.CorruptEventsLog;
    if (version_val != .integer) return error.CorruptEventsLog;
    const version_i = version_val.integer;
    if (version_i < 0) return error.CorruptEventsLog;
    const version: u32 = std.math.cast(u32, version_i) orelse return error.SchemaVersionTooNew;
    if (version > events.schema_version) return error.SchemaVersionTooNew;

    const kind_val = obj.get("k") orelse return error.CorruptEventsLog;
    if (kind_val != .string) return error.CorruptEventsLog;

    const payload = obj.get("d") orelse return error.CorruptEventsLog;

    const kind = kind_val.string;
    if (std.mem.eql(u8, kind, "user_text")) {
        try appendText(allocator, tr, payload, .user_text);
    } else if (std.mem.eql(u8, kind, "model_text")) {
        try appendText(allocator, tr, payload, .model_text);
    } else if (std.mem.eql(u8, kind, "proof_card")) {
        try appendText(allocator, tr, payload, .proof_card);
    } else if (std.mem.eql(u8, kind, "diagnostic_box")) {
        try appendText(allocator, tr, payload, .diagnostic_box);
    } else if (std.mem.eql(u8, kind, "tool_use")) {
        try appendToolUse(allocator, tr, payload);
    } else if (std.mem.eql(u8, kind, "tool_result")) {
        try appendToolResult(allocator, tr, payload);
    } else if (std.mem.eql(u8, kind, "system_note")) {
        try appendText(allocator, tr, payload, .system_note);
    } else {
        return error.CorruptEventsLog;
    }
}

const TextKind = enum { user_text, model_text, proof_card, diagnostic_box, system_note };

fn appendText(
    allocator: std.mem.Allocator,
    tr: *transcript.Transcript,
    payload: std.json.Value,
    kind: TextKind,
) !void {
    if (payload != .string) return error.CorruptEventsLog;
    const body = try allocator.dupe(u8, payload.string);
    errdefer allocator.free(body);

    const entry: transcript.OwnedEntry = switch (kind) {
        .user_text => .{ .user_text = body },
        .model_text => .{ .model_text = body },
        .proof_card => .{ .proof_card = body },
        .diagnostic_box => .{ .diagnostic_box = body },
        .system_note => .{ .system_note = body },
    };
    try tr.entries.append(allocator, entry);
}

fn appendToolUse(
    allocator: std.mem.Allocator,
    tr: *transcript.Transcript,
    payload: std.json.Value,
) !void {
    if (payload != .object) return error.CorruptEventsLog;
    const obj = payload.object;

    const id_val = obj.get("id") orelse return error.CorruptEventsLog;
    const name_val = obj.get("name") orelse return error.CorruptEventsLog;
    const args_val = obj.get("args_json") orelse return error.CorruptEventsLog;
    if (id_val != .string or name_val != .string or args_val != .string) {
        return error.CorruptEventsLog;
    }

    const calls = try allocator.alloc(transcript.OwnedToolCall, 1);
    errdefer allocator.free(calls);

    const id_copy = try allocator.dupe(u8, id_val.string);
    errdefer allocator.free(id_copy);
    const name_copy = try allocator.dupe(u8, name_val.string);
    errdefer allocator.free(name_copy);
    const args_copy = try allocator.dupe(u8, args_val.string);
    errdefer allocator.free(args_copy);

    calls[0] = .{ .id = id_copy, .name = name_copy, .args_json = args_copy };
    try tr.entries.append(allocator, .{ .assistant_tool_use = calls });
}

fn appendToolResult(
    allocator: std.mem.Allocator,
    tr: *transcript.Transcript,
    payload: std.json.Value,
) !void {
    if (payload != .object) return error.CorruptEventsLog;
    const obj = payload.object;

    const tu_id_val = obj.get("tool_use_id") orelse return error.CorruptEventsLog;
    const tool_name_val = obj.get("tool_name") orelse return error.CorruptEventsLog;
    const ok_val = obj.get("ok") orelse return error.CorruptEventsLog;
    const body_val = obj.get("body") orelse return error.CorruptEventsLog;
    if (tu_id_val != .string or tool_name_val != .string or ok_val != .bool or body_val != .string) {
        return error.CorruptEventsLog;
    }

    const tu_id_copy = try allocator.dupe(u8, tu_id_val.string);
    errdefer allocator.free(tu_id_copy);
    const tool_name_copy = try allocator.dupe(u8, tool_name_val.string);
    errdefer allocator.free(tool_name_copy);
    const body_copy = try allocator.dupe(u8, body_val.string);
    errdefer allocator.free(body_copy);

    try tr.entries.append(allocator, .{ .tool_result = .{
        .tool_use_id = tu_id_copy,
        .tool_name = tool_name_copy,
        .ok = ok_val.bool,
        .body = body_copy,
    } });
}

// ===========================================================================
// Tests
// ===========================================================================

const testing = std.testing;

const IsolatedTmp = @import("../test_support/tmp.zig").IsolatedTmp;

fn initTmp(allocator: std.mem.Allocator) !IsolatedTmp {
    return IsolatedTmp.init(allocator, "reconstructor");
}

test "reconstructTranscript returns an empty Transcript for an empty file" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try zigts.file_io.writeFile(allocator, path, "");

    var tr = try reconstructTranscript(allocator, path, null);
    defer tr.deinit(allocator);

    try testing.expectEqual(@as(usize, 0), tr.len());
}

test "reconstructTranscript propagates FileNotFound for a missing log" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "does-not-exist.jsonl");
    defer allocator.free(path);

    const result = reconstructTranscript(allocator, path, null);
    try testing.expectError(error.FileNotFound, result);
}

test "reconstructTranscript round-trips user_text, model_text, tool_use, tool_result, proof_card" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try events.appendEvent(allocator, path, .{ .user_text = "hi" });
    try events.appendEvent(allocator, path, .{ .model_text = "hello back" });
    try events.appendEvent(allocator, path, .{ .tool_use = .{
        .id = "toolu_1",
        .name = "zigts_expert_meta",
        .args_json = "{\"verbose\":true}",
    } });
    try events.appendEvent(allocator, path, .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .body = "{\"ok\":true}",
    } });
    try events.appendEvent(allocator, path, .{ .proof_card = "contract ok" });

    var tr = try reconstructTranscript(allocator, path, null);
    defer tr.deinit(allocator);

    try testing.expectEqual(@as(usize, 5), tr.len());

    switch (tr.at(0).*) {
        .user_text => |body| try testing.expectEqualStrings("hi", body),
        else => return error.TestFailed,
    }
    switch (tr.at(1).*) {
        .model_text => |body| try testing.expectEqualStrings("hello back", body),
        else => return error.TestFailed,
    }
    switch (tr.at(2).*) {
        .assistant_tool_use => |calls| {
            try testing.expectEqual(@as(usize, 1), calls.len);
            try testing.expectEqualStrings("toolu_1", calls[0].id);
            try testing.expectEqualStrings("zigts_expert_meta", calls[0].name);
            try testing.expectEqualStrings("{\"verbose\":true}", calls[0].args_json);
        },
        else => return error.TestFailed,
    }
    switch (tr.at(3).*) {
        .tool_result => |r| {
            try testing.expectEqualStrings("toolu_1", r.tool_use_id);
            try testing.expectEqualStrings("zigts_expert_meta", r.tool_name);
            try testing.expect(r.ok);
            try testing.expectEqualStrings("{\"ok\":true}", r.body);
        },
        else => return error.TestFailed,
    }
    switch (tr.at(4).*) {
        .proof_card => |body| try testing.expectEqualStrings("contract ok", body),
        else => return error.TestFailed,
    }
}

test "reconstructTranscript rejects schema versions newer than this binary" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try zigts.file_io.writeFile(allocator, path, "{\"v\":9999,\"k\":\"user_text\",\"d\":\"x\"}\n");

    var diag: Diagnostic = .{};
    const result = reconstructTranscript(allocator, path, &diag);
    try testing.expectError(error.SchemaVersionTooNew, result);
}

test "reconstructTranscript reports line 1 on a truncated first line" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try zigts.file_io.writeFile(allocator, path, "{\"v\":1,\"k\":\"user_text\",\"d\"\n");

    var diag: Diagnostic = .{};
    const result = reconstructTranscript(allocator, path, &diag);
    try testing.expectError(error.CorruptEventsLog, result);
    try testing.expectEqual(@as(usize, 1), diag.line_number);
}

test "reconstructTranscript reports the line number of the first corrupt record" {
    const allocator = testing.allocator;
    var tmp = try initTmp(allocator);
    defer tmp.cleanup(allocator);

    const path = try tmp.childPath(allocator, "events.jsonl");
    defer allocator.free(path);

    try events.appendEvent(allocator, path, .{ .user_text = "one" });
    try events.appendEvent(allocator, path, .{ .user_text = "two" });
    // Append a hand-rolled garbage line so line 3 fails parsing.
    const fd = try zigts.file_io.openAppend(allocator, path);
    defer std.Io.Threaded.closeFd(fd);
    const junk = "not json at all\n";
    _ = std.c.write(fd, junk.ptr, junk.len);

    var diag: Diagnostic = .{};
    const result = reconstructTranscript(allocator, path, &diag);
    try testing.expectError(error.CorruptEventsLog, result);
    try testing.expectEqual(@as(usize, 3), diag.line_number);
}
