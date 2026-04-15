//! Pure state machine for the raw-mode input line. Takes KeyEvents, mutates
//! an owned buffer, and returns an Action telling the caller what to do next.
//! No I/O — the app loop drives both ends.

const std = @import("std");

pub const KeyKind = enum {
    char,
    backspace,
    enter,
    ctrl_c,
    eof,
    ignore,
};

pub const KeyEvent = struct {
    kind: KeyKind,
    byte: u8 = 0,
};

pub const Action = enum {
    /// No visible effect; the app loop does nothing.
    none,
    /// The buffer or cursor changed; the app loop should redraw the prompt line.
    redraw,
    /// The user committed the current line; the app loop should dispatch it.
    submit,
    /// The user wants to exit the REPL.
    cancel,
};

pub const LineEditor = struct {
    buffer: std.ArrayList(u8) = .empty,

    pub fn deinit(self: *LineEditor, allocator: std.mem.Allocator) void {
        self.buffer.deinit(allocator);
    }

    pub fn line(self: *const LineEditor) []const u8 {
        return self.buffer.items;
    }

    pub fn clear(self: *LineEditor) void {
        self.buffer.clearRetainingCapacity();
    }

    pub fn handle(
        self: *LineEditor,
        allocator: std.mem.Allocator,
        event: KeyEvent,
    ) !Action {
        return switch (event.kind) {
            .char => blk: {
                try self.buffer.append(allocator, event.byte);
                break :blk .redraw;
            },
            .backspace => blk: {
                if (self.buffer.items.len == 0) break :blk .none;
                _ = self.buffer.pop();
                break :blk .redraw;
            },
            .enter => .submit,
            .ctrl_c, .eof => .cancel,
            .ignore => .none,
        };
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn keyChar(b: u8) KeyEvent {
    return .{ .kind = .char, .byte = b };
}

fn typeString(editor: *LineEditor, allocator: std.mem.Allocator, s: []const u8) !void {
    for (s) |b| _ = try editor.handle(allocator, keyChar(b));
}

test "typing appends characters and returns redraw" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    const action_h = try editor.handle(testing.allocator, keyChar('h'));
    try testing.expectEqual(Action.redraw, action_h);

    try typeString(&editor, testing.allocator, "ello");
    try testing.expectEqualStrings("hello", editor.line());
}

test "backspace removes the last character" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "hello");
    const action = try editor.handle(testing.allocator, .{ .kind = .backspace });
    try testing.expectEqual(Action.redraw, action);
    try testing.expectEqualStrings("hell", editor.line());
}

test "backspace on empty buffer is a no-op" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    const action = try editor.handle(testing.allocator, .{ .kind = .backspace });
    try testing.expectEqual(Action.none, action);
}

test "enter returns submit without clearing the buffer" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "zigts_expert_meta");
    const action = try editor.handle(testing.allocator, .{ .kind = .enter });
    try testing.expectEqual(Action.submit, action);
    try testing.expectEqualStrings("zigts_expert_meta", editor.line());
}

test "clear resets the buffer" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "abc");
    editor.clear();
    try testing.expectEqualStrings("", editor.line());
    try typeString(&editor, testing.allocator, "xyz");
    try testing.expectEqualStrings("xyz", editor.line());
}

test "ctrl_c and eof both return cancel" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    const ctrl_c_action = try editor.handle(testing.allocator, .{ .kind = .ctrl_c });
    try testing.expectEqual(Action.cancel, ctrl_c_action);

    const eof_action = try editor.handle(testing.allocator, .{ .kind = .eof });
    try testing.expectEqual(Action.cancel, eof_action);
}

test "ignore events are silent no-ops" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "ab");
    const action = try editor.handle(testing.allocator, .{ .kind = .ignore });
    try testing.expectEqual(Action.none, action);
    try testing.expectEqualStrings("ab", editor.line());
}

test "type, submit, clear, type again (full dispatch cycle)" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "foo");
    const submit_a = try editor.handle(testing.allocator, .{ .kind = .enter });
    try testing.expectEqual(Action.submit, submit_a);
    try testing.expectEqualStrings("foo", editor.line());

    editor.clear();
    try typeString(&editor, testing.allocator, "bar baz");
    try testing.expectEqualStrings("bar baz", editor.line());

    const submit_b = try editor.handle(testing.allocator, .{ .kind = .enter });
    try testing.expectEqual(Action.submit, submit_b);
}
