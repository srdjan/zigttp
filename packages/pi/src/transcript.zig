//! Minimal ownership-safe transcript of `turn.Message` values. The state
//! machine's `render` action hands the loop a Message holding borrowed
//! slices; the transcript dupes the body so the source (veto envelope,
//! tool body, etc.) can be freed independently. A plain-text renderer
//! flattens each variant to a label-prefixed line so the REPL / raw-mode
//! TUI have a single path to visible output before the widget layer lands.

const std = @import("std");
const turn = @import("turn.zig");

/// Aliased to the inferred tag of `turn.Message` so adding a variant there
/// automatically flows through here; the `ownMessage` switch below catches
/// any missed arm at compile time.
pub const Tag = std.meta.Tag(turn.Message);

pub const OwnedMessage = struct {
    tag: Tag,
    body: []const u8,

    pub fn deinit(self: *OwnedMessage, allocator: std.mem.Allocator) void {
        allocator.free(self.body);
        self.body = &.{};
    }
};

pub const Transcript = struct {
    entries: std.ArrayListUnmanaged(OwnedMessage) = .empty,

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
        const owned = try ownMessage(allocator, message);
        errdefer allocator.free(owned.body);
        try self.entries.append(allocator, owned);
    }

    pub fn len(self: *const Transcript) usize {
        return self.entries.items.len;
    }

    pub fn at(self: *const Transcript, index: usize) *const OwnedMessage {
        return &self.entries.items[index];
    }
};

fn ownMessage(allocator: std.mem.Allocator, message: turn.Message) !OwnedMessage {
    const pair: struct { tag: Tag, body: []const u8 } = switch (message) {
        .user_text => |b| .{ .tag = .user_text, .body = b },
        .model_text => |b| .{ .tag = .model_text, .body = b },
        .proof_card => |b| .{ .tag = .proof_card, .body = b },
        .diagnostic_box => |b| .{ .tag = .diagnostic_box, .body = b },
        .tool_result => |b| .{ .tag = .tool_result, .body = b },
    };
    const owned_body = try allocator.dupe(u8, pair.body);
    return .{ .tag = pair.tag, .body = owned_body };
}

/// Always terminates with LF so successive messages don't collapse onto one
/// line. Same invariant as `printBody` in `tui/app.zig`.
pub fn renderPlain(writer: anytype, entry: *const OwnedMessage) !void {
    const label: []const u8 = switch (entry.tag) {
        .user_text => "user",
        .model_text => "model",
        .proof_card => "proof",
        .diagnostic_box => "error",
        .tool_result => "tool",
    };
    try writer.writeAll(label);
    try writer.writeAll(": ");
    try writer.writeAll(entry.body);
    if (entry.body.len == 0 or entry.body[entry.body.len - 1] != '\n') {
        try writer.writeAll("\n");
    }
}

pub fn renderAll(writer: anytype, transcript: *const Transcript) !void {
    for (transcript.entries.items) |*entry| {
        try renderPlain(writer, entry);
    }
}

pub fn renderEntryToOwned(
    allocator: std.mem.Allocator,
    entry: *const OwnedMessage,
) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try renderPlain(&aw.writer, entry);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(allocator);
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

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

test "empty transcript renders to empty string" {
    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    const out = try renderToString(testing.allocator, &tr);
    defer testing.allocator.free(out);

    try testing.expectEqualStrings("", out);
}

test "append dupes the source so the caller can free it" {
    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    {
        // Scope-local source; transcript must not outlive a borrow into it.
        var scratch = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
        try tr.append(testing.allocator, .{ .user_text = scratch[0..] });
        scratch[0] = 'X'; // Mutate the source after append.
    }

    try testing.expectEqual(@as(usize, 1), tr.len());
    try testing.expectEqualStrings("hello", tr.at(0).body);
}

test "every Message variant renders with its own label" {
    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    try tr.append(testing.allocator, .{ .user_text = "add a route" });
    try tr.append(testing.allocator, .{ .model_text = "here is the plan" });
    try tr.append(testing.allocator, .{ .proof_card = "contract ok" });
    try tr.append(testing.allocator, .{ .diagnostic_box = "ZTS001 unsupported var" });
    try tr.append(testing.allocator, .{ .tool_result = "{\"ok\":true}" });

    const out = try renderToString(testing.allocator, &tr);
    defer testing.allocator.free(out);

    try testing.expect(std.mem.indexOf(u8, out, "user: add a route\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "model: here is the plan\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "proof: contract ok\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "error: ZTS001 unsupported var\n") != null);
    try testing.expect(std.mem.indexOf(u8, out, "tool: {\"ok\":true}\n") != null);
}

test "renderPlain does not double-terminate a body that already ends in LF" {
    var tr: Transcript = .{};
    defer tr.deinit(testing.allocator);

    try tr.append(testing.allocator, .{ .tool_result = "line\n" });

    const out = try renderToString(testing.allocator, &tr);
    defer testing.allocator.free(out);

    try testing.expectEqualStrings("tool: line\n", out);
}

test "veto -> turn -> transcript pipeline: clean handler produces a proof line" {
    // The full phase-2 spine wired end-to-end with a real compiler run:
    //   runVeto        -> EditOutcome
    //   TurnMachine    -> Action.render(Message.proof_card)
    //   Transcript     -> OwnedMessage + renderPlain
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

    try testing.expectEqual(@as(usize, 1), tr.len());
    try testing.expectEqual(Tag.proof_card, tr.at(0).tag);

    const out = try renderToString(testing.allocator, &tr);
    defer testing.allocator.free(out);

    try testing.expect(std.mem.startsWith(u8, out, "proof: "));
    try testing.expect(std.mem.indexOf(u8, out, "\"total\":0") != null);
}

test "veto -> turn -> transcript pipeline: broken handler produces an error line" {
    var outcome = try veto.runVeto(testing.allocator, .{
        .file = "handler.ts",
        .content = "function handler(req: Request): Response { var x = 1; return Response.json({x}); }",
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

    try testing.expectEqual(Tag.diagnostic_box, tr.at(0).tag);

    const out = try renderToString(testing.allocator, &tr);
    defer testing.allocator.free(out);

    try testing.expect(std.mem.startsWith(u8, out, "error: "));
    try testing.expect(std.mem.indexOf(u8, out, "\"ZTS001\"") != null);
}
