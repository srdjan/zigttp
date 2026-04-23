//! Ownership-safe transcript for user text, assistant narration, structured
//! tool-use, tool results, and visible verification output.

const std = @import("std");
const turn = @import("turn.zig");
const ui_payload = @import("ui_payload.zig");
const box_widget = @import("tui/widgets/box.zig");

/// Return an allocator-owned copy of `body`, capped at `max` bytes. When
/// truncation happens, the returned slice ends with a note recording how many
/// bytes were dropped. Callers always own the result, giving persistence
/// workers a uniform lifetime regardless of input size.
///
/// If `max` is smaller than the truncation suffix itself, the function falls
/// back to a short fixed marker so it never crashes on tiny `max` values.
pub fn capToolResultBody(
    allocator: std.mem.Allocator,
    body: []const u8,
    max: usize,
) ![]u8 {
    if (body.len <= max) return try allocator.dupe(u8, body);

    const suffix_prefix = "\n...[truncated ";
    const suffix_tail = " bytes]";

    // Render the digit count against an upper bound on dropped bytes so the
    // suffix width is known before sizing the keep slice. body.len fits in
    // usize, which prints in at most 20 decimal digits.
    var num_buf: [20]u8 = undefined;
    const digits = std.fmt.bufPrint(&num_buf, "{d}", .{body.len}) catch unreachable;
    const suffix_len = suffix_prefix.len + digits.len + suffix_tail.len;

    if (max < suffix_len) return try allocator.dupe(u8, "...[truncated]");

    const keep = max - suffix_len;
    const dropped = body.len - keep;
    const real_digits = std.fmt.bufPrint(&num_buf, "{d}", .{dropped}) catch unreachable;
    const real_suffix_len = suffix_prefix.len + real_digits.len + suffix_tail.len;
    const total = keep + real_suffix_len;

    const out = try allocator.alloc(u8, total);
    @memcpy(out[0..keep], body[0..keep]);
    @memcpy(out[keep .. keep + suffix_prefix.len], suffix_prefix);
    @memcpy(
        out[keep + suffix_prefix.len .. keep + suffix_prefix.len + real_digits.len],
        real_digits,
    );
    @memcpy(out[keep + suffix_prefix.len + real_digits.len ..], suffix_tail);
    return out;
}

pub const OwnedToolCall = struct {
    id: []const u8,
    name: []const u8,
    args_json: []const u8,

    pub fn deinit(self: *OwnedToolCall, allocator: std.mem.Allocator) void {
        allocator.free(self.id);
        allocator.free(self.name);
        allocator.free(self.args_json);
        self.* = .{ .id = &.{}, .name = &.{}, .args_json = &.{} };
    }
};

pub const OwnedToolResult = struct {
    tool_use_id: []const u8,
    tool_name: []const u8,
    ok: bool,
    llm_text: []const u8,
    ui_payload: ?ui_payload.UiPayload = null,

    pub fn deinit(self: *OwnedToolResult, allocator: std.mem.Allocator) void {
        allocator.free(self.tool_use_id);
        allocator.free(self.tool_name);
        allocator.free(self.llm_text);
        if (self.ui_payload) |*payload| payload.deinit(allocator);
        self.* = .{
            .tool_use_id = &.{},
            .tool_name = &.{},
            .ok = false,
            .llm_text = &.{},
            .ui_payload = null,
        };
    }
};

pub const OwnedDisplayMessage = struct {
    llm_text: []const u8,
    ui_payload: ?ui_payload.UiPayload = null,

    pub fn deinit(self: *OwnedDisplayMessage, allocator: std.mem.Allocator) void {
        allocator.free(self.llm_text);
        if (self.ui_payload) |*payload| payload.deinit(allocator);
        self.* = .{ .llm_text = &.{}, .ui_payload = null };
    }
};

pub const OwnedEntry = union(enum) {
    user_text: []const u8,
    model_text: []const u8,
    assistant_tool_use: []OwnedToolCall,
    proof_card: OwnedDisplayMessage,
    diagnostic_box: OwnedDisplayMessage,
    tool_result: OwnedToolResult,
    system_note: []const u8,

    pub fn deinit(self: *OwnedEntry, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .user_text => |body| allocator.free(body),
            .model_text => |body| allocator.free(body),
            .proof_card => |*message| message.deinit(allocator),
            .diagnostic_box => |*message| message.deinit(allocator),
            .system_note => |body| allocator.free(body),
            .assistant_tool_use => |calls| {
                for (calls) |*call| call.deinit(allocator);
                allocator.free(calls);
            },
            .tool_result => |*result| result.deinit(allocator),
        }
        self.* = .{ .user_text = &.{} };
    }
};

pub const Tag = std.meta.Tag(OwnedEntry);

pub const Transcript = struct {
    entries: std.ArrayListUnmanaged(OwnedEntry) = .empty,

    pub fn deinit(self: *Transcript, allocator: std.mem.Allocator) void {
        for (self.entries.items) |*entry| entry.deinit(allocator);
        self.entries.deinit(allocator);
        self.* = .{};
    }

    pub fn append(
        self: *Transcript,
        allocator: std.mem.Allocator,
        message: turn.Message,
    ) !void {
        try self.entries.append(allocator, try ownMessage(allocator, message));
    }

    pub fn len(self: *const Transcript) usize {
        return self.entries.items.len;
    }

    pub fn at(self: *const Transcript, index: usize) *const OwnedEntry {
        return &self.entries.items[index];
    }
};

fn ownMessage(allocator: std.mem.Allocator, message: turn.Message) !OwnedEntry {
    return switch (message) {
        .user_text => |body| .{ .user_text = try allocator.dupe(u8, body) },
        .model_text => |body| .{ .model_text = try allocator.dupe(u8, body) },
        .proof_card => |body| .{ .proof_card = try ownDisplayMessage(allocator, body) },
        .diagnostic_box => |body| .{ .diagnostic_box = try ownDisplayMessage(allocator, body) },
        .assistant_tool_use => |calls| blk: {
            const owned = try allocator.alloc(OwnedToolCall, calls.len);
            errdefer allocator.free(owned);
            for (calls, 0..) |call, i| {
                owned[i] = .{
                    .id = try allocator.dupe(u8, call.id),
                    .name = try allocator.dupe(u8, call.name),
                    .args_json = try allocator.dupe(u8, call.args_json),
                };
            }
            break :blk .{ .assistant_tool_use = owned };
        },
        .tool_result => |result| .{ .tool_result = .{
            .tool_use_id = try allocator.dupe(u8, result.tool_use_id),
            .tool_name = try allocator.dupe(u8, result.tool_name),
            .ok = result.ok,
            .llm_text = try allocator.dupe(u8, result.llm_text),
            .ui_payload = if (result.ui_payload) |payload|
                try payload.clone(allocator)
            else
                null,
        } },
        .system_note => |body| .{ .system_note = try allocator.dupe(u8, body) },
    };
}

fn ownDisplayMessage(
    allocator: std.mem.Allocator,
    message: turn.DisplayMessage,
) !OwnedDisplayMessage {
    return .{
        .llm_text = try allocator.dupe(u8, message.llm_text),
        .ui_payload = if (message.ui_payload) |payload|
            try payload.clone(allocator)
        else
            null,
    };
}

pub fn renderPlain(writer: anytype, entry: *const OwnedEntry) !void {
    switch (entry.*) {
        .user_text => |body| try writeTaggedLine(writer, "user", body),
        .model_text => |body| try writeTaggedLine(writer, "model", body),
        .proof_card => |message| try writeTaggedLine(writer, "proof", message.llm_text),
        .diagnostic_box => |message| try writeTaggedLine(writer, "error", message.llm_text),
        .system_note => |body| try writeTaggedLine(writer, "note", body),
        .assistant_tool_use => |calls| {
            try writer.writeAll("assistant: tool_use ");
            for (calls, 0..) |call, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(call.name);
            }
            try writer.writeAll("\n");
        },
        .tool_result => |result| {
            try writer.writeAll("tool ");
            try writer.writeAll(result.tool_name);
            try writer.writeAll(": ");
            try writer.writeAll(result.llm_text);
            if (result.llm_text.len == 0 or result.llm_text[result.llm_text.len - 1] != '\n') {
                try writer.writeAll("\n");
            }
        },
    }
}

fn writeTaggedLine(writer: anytype, label: []const u8, body: []const u8) !void {
    try writer.writeAll(label);
    try writer.writeAll(": ");
    try writer.writeAll(body);
    if (body.len == 0 or body[body.len - 1] != '\n') {
        try writer.writeAll("\n");
    }
}

fn renderAll(writer: anytype, transcript: *const Transcript) !void {
    for (transcript.entries.items) |*entry| {
        try renderPlain(writer, entry);
    }
}

fn renderRich(writer: anytype, entry: *const OwnedEntry) !void {
    switch (entry.*) {
        .proof_card => |message| try box_widget.writeBox(
            writer,
            .{ .title = "proof", .color = .green },
            message.llm_text,
        ),
        .diagnostic_box => |message| try box_widget.writeBox(
            writer,
            .{ .title = "veto", .color = .red },
            message.llm_text,
        ),
        else => try renderPlain(writer, entry),
    }
}

pub fn renderRichEntryToOwned(
    allocator: std.mem.Allocator,
    entry: *const OwnedEntry,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try renderRich(&aw.writer, entry);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

const testing = std.testing;
const veto = @import("veto.zig");

fn renderToString(
    allocator: std.mem.Allocator,
    transcript: *const Transcript,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try renderAll(&aw.writer, transcript);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

test "append dupes textual message bodies" {
    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    {
        var scratch = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
        try tr.append(testing.allocator, .{ .user_text = scratch[0..] });
        scratch[0] = 'X';
    }

    switch (tr.at(0).*) {
        .user_text => |body| try testing.expectEqualStrings("hello", body),
        else => return error.TestFailed,
    }
}

test "assistant_tool_use and tool_result variants are preserved" {
    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_1", .name = "zigts_expert_meta", .args_json = "{}" },
        .{ .id = "toolu_2", .name = "zigts_expert_features", .args_json = "{}" },
    };
    try tr.append(testing.allocator, .{ .assistant_tool_use = &calls });
    try tr.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .llm_text = "{\"ok\":true}\n",
    } });

    switch (tr.at(0).*) {
        .assistant_tool_use => |owned_calls| {
            try testing.expectEqual(@as(usize, 2), owned_calls.len);
            try testing.expectEqualStrings("zigts_expert_meta", owned_calls[0].name);
        },
        else => return error.TestFailed,
    }
    switch (tr.at(1).*) {
        .tool_result => |result| {
            try testing.expect(result.ok);
            try testing.expectEqualStrings("toolu_1", result.tool_use_id);
        },
        else => return error.TestFailed,
    }
}

test "every entry variant renders a stable plain-text label" {
    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_1", .name = "zigts_expert_meta", .args_json = "{}" },
    };
    try tr.append(testing.allocator, .{ .user_text = "add a route" });
    try tr.append(testing.allocator, .{ .model_text = "I'll inspect first." });
    try tr.append(testing.allocator, .{ .assistant_tool_use = &calls });
    try tr.append(testing.allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .llm_text = "{\"ok\":true}",
    } });
    try tr.append(testing.allocator, .{ .proof_card = .{ .llm_text = "contract ok" } });
    try tr.append(testing.allocator, .{ .diagnostic_box = .{ .llm_text = "ZTS001 unsupported var" } });

    const out = try renderToString(testing.allocator, &tr);
    defer testing.allocator.free(out);

    try testing.expect(std.mem.indexOf(u8, out, "user: add a route\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "model: I'll inspect first.\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "assistant: tool_use zigts_expert_meta\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "tool zigts_expert_meta: {\"ok\":true}\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "proof: contract ok\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "error: ZTS001 unsupported var\n") != null);
}

test "capToolResultBody returns original body when under the cap" {
    const out = try capToolResultBody(testing.allocator, "hello", 100);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("hello", out);
}

test "capToolResultBody returns original body when exactly at the cap" {
    const out = try capToolResultBody(testing.allocator, "hello", 5);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("hello", out);
}

test "capToolResultBody truncates with a byte-count suffix when over the cap" {
    var big: [1000]u8 = undefined;
    @memset(&big, 'a');

    const out = try capToolResultBody(testing.allocator, &big, 100);
    defer testing.allocator.free(out);

    try testing.expect(out.len <= 100);
    try testing.expect(std.mem.indexOf(u8, out, "[truncated") != null);

    // The suffix must encode the exact number of dropped bytes.
    const suffix_prefix = "\n...[truncated ";
    const idx = std.mem.indexOf(u8, out, suffix_prefix) orelse return error.TestFailed;
    const digits_start = idx + suffix_prefix.len;
    const digits_end = std.mem.indexOfScalarPos(u8, out, digits_start, ' ') orelse return error.TestFailed;
    const dropped = try std.fmt.parseInt(usize, out[digits_start..digits_end], 10);
    try testing.expectEqual(big.len - idx, dropped);
}

test "veto -> turn -> transcript pipeline still lands a proof entry" {
    var outcome = try veto.runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { return Response.json({ok: true}); }",
        .before = null,
    });
    defer outcome.deinit(testing.allocator);

    var machine: turn.TurnMachine = .{ .state = .verifying_edit };
    const action = machine.transition(.{ .edit_verified = outcome });

    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    switch (action) {
        .render => |msg| try tr.append(testing.allocator, msg),
        else => return error.TestFailed,
    }

    switch (tr.at(0).*) {
        .proof_card => |message| try testing.expect(std.mem.indexOf(u8, message.llm_text, "\"total\":0") != null),
        else => return error.TestFailed,
    }
}
