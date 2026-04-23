//! Pure state machine for the raw-mode input line. Takes KeyEvents, mutates
//! an owned buffer, and returns an Action telling the caller what to do next.
//! No I/O — the app loop drives both ends.

const std = @import("std");

pub const KeyKind = enum {
    char,
    backspace,
    delete,
    left,
    right,
    up,
    down,
    home,
    end,
    enter,
    tab,
    shift_tab,
    esc,
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

/// Navigation state for history recall. `idle` is the normal editing
/// case. `browsing` captures both the current history index and the
/// caller's in-progress draft, so the two sentinels live together and
/// exiting history mode is a single assignment.
const HistoryNav = union(enum) {
    idle,
    browsing: struct { index: usize, draft: ?[]u8 },
};

pub const LineEditor = struct {
    buffer: std.ArrayList(u8) = .empty,
    history: std.ArrayList([]u8) = .empty,
    cursor_index: usize = 0,
    nav: HistoryNav = .idle,

    pub fn deinit(self: *LineEditor, allocator: std.mem.Allocator) void {
        self.buffer.deinit(allocator);
        for (self.history.items) |entry| allocator.free(entry);
        self.history.deinit(allocator);
        self.exitHistory(allocator);
    }

    pub fn line(self: *const LineEditor) []const u8 {
        return self.buffer.items;
    }

    pub fn cursor(self: *const LineEditor) usize {
        return self.cursor_index;
    }

    pub fn clear(self: *LineEditor, allocator: std.mem.Allocator) void {
        self.buffer.clearRetainingCapacity();
        self.cursor_index = 0;
        self.exitHistory(allocator);
    }

    pub fn handle(
        self: *LineEditor,
        allocator: std.mem.Allocator,
        event: KeyEvent,
    ) !Action {
        return switch (event.kind) {
            .char => try self.insertChar(allocator, event.byte),
            .backspace => self.backspace(allocator),
            .delete => self.delete(allocator),
            .left => self.moveLeft(),
            .right => self.moveRight(),
            .home => self.moveHome(),
            .end => self.moveEnd(),
            .up => try self.historyUp(allocator),
            .down => try self.historyDown(allocator),
            .enter => try self.submitCurrent(allocator),
            .tab, .shift_tab, .esc => .none,
            .ctrl_c, .eof => .cancel,
            .ignore => .none,
        };
    }

    fn insertChar(self: *LineEditor, allocator: std.mem.Allocator, byte: u8) !Action {
        self.exitHistory(allocator);
        const len = self.buffer.items.len;
        try self.buffer.resize(allocator, len + 1);
        std.mem.copyBackwards(u8, self.buffer.items[self.cursor_index + 1 ..], self.buffer.items[self.cursor_index..len]);
        self.buffer.items[self.cursor_index] = byte;
        self.cursor_index += 1;
        return .redraw;
    }

    fn backspace(self: *LineEditor, allocator: std.mem.Allocator) Action {
        self.exitHistory(allocator);
        if (self.cursor_index == 0) return .none;
        const len = self.buffer.items.len;
        std.mem.copyForwards(u8, self.buffer.items[self.cursor_index - 1 .. len - 1], self.buffer.items[self.cursor_index..len]);
        self.buffer.items = self.buffer.items[0 .. len - 1];
        self.cursor_index -= 1;
        return .redraw;
    }

    fn delete(self: *LineEditor, allocator: std.mem.Allocator) Action {
        self.exitHistory(allocator);
        if (self.cursor_index >= self.buffer.items.len) return .none;
        const len = self.buffer.items.len;
        std.mem.copyForwards(u8, self.buffer.items[self.cursor_index .. len - 1], self.buffer.items[self.cursor_index + 1 .. len]);
        self.buffer.items = self.buffer.items[0 .. len - 1];
        return .redraw;
    }

    fn moveLeft(self: *LineEditor) Action {
        if (self.cursor_index == 0) return .none;
        self.cursor_index -= 1;
        return .redraw;
    }

    fn moveRight(self: *LineEditor) Action {
        if (self.cursor_index >= self.buffer.items.len) return .none;
        self.cursor_index += 1;
        return .redraw;
    }

    fn moveHome(self: *LineEditor) Action {
        if (self.cursor_index == 0) return .none;
        self.cursor_index = 0;
        return .redraw;
    }

    fn moveEnd(self: *LineEditor) Action {
        if (self.cursor_index == self.buffer.items.len) return .none;
        self.cursor_index = self.buffer.items.len;
        return .redraw;
    }

    fn historyUp(self: *LineEditor, allocator: std.mem.Allocator) !Action {
        if (self.history.items.len == 0) return .none;
        const next_index = switch (self.nav) {
            .idle => self.history.items.len - 1,
            .browsing => |b| if (b.index > 0) b.index - 1 else return .none,
        };
        if (self.nav == .idle) {
            const draft: ?[]u8 = if (self.buffer.items.len > 0)
                try allocator.dupe(u8, self.buffer.items)
            else
                null;
            self.nav = .{ .browsing = .{ .index = next_index, .draft = draft } };
        } else {
            self.nav.browsing.index = next_index;
        }
        try self.replaceBuffer(allocator, self.history.items[next_index]);
        return .redraw;
    }

    fn historyDown(self: *LineEditor, allocator: std.mem.Allocator) !Action {
        const current = switch (self.nav) {
            .idle => return .none,
            .browsing => |*b| b,
        };
        if (current.index + 1 < self.history.items.len) {
            current.index += 1;
            try self.replaceBuffer(allocator, self.history.items[current.index]);
            return .redraw;
        }
        // Fell off the end of history: restore the draft the user was
        // editing before entering browse mode.
        const draft = current.draft;
        if (draft) |d| {
            try self.replaceBuffer(allocator, d);
        } else {
            self.buffer.clearRetainingCapacity();
            self.cursor_index = 0;
        }
        self.exitHistory(allocator);
        return .redraw;
    }

    fn submitCurrent(self: *LineEditor, allocator: std.mem.Allocator) !Action {
        if (self.buffer.items.len > 0) {
            const should_store = if (self.history.items.len == 0)
                true
            else
                !std.mem.eql(u8, self.history.items[self.history.items.len - 1], self.buffer.items);
            if (should_store) {
                try self.history.append(allocator, try allocator.dupe(u8, self.buffer.items));
            }
        }
        self.exitHistory(allocator);
        return .submit;
    }

    fn replaceBuffer(self: *LineEditor, allocator: std.mem.Allocator, value: []const u8) !void {
        self.buffer.clearRetainingCapacity();
        try self.buffer.appendSlice(allocator, value);
        self.cursor_index = self.buffer.items.len;
    }

    /// Transition to `.idle`, freeing any stashed draft. Safe to call
    /// when already idle.
    fn exitHistory(self: *LineEditor, allocator: std.mem.Allocator) void {
        switch (self.nav) {
            .idle => {},
            .browsing => |b| if (b.draft) |d| allocator.free(d),
        }
        self.nav = .idle;
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
    try testing.expectEqual(@as(usize, 5), editor.cursor());
}

test "backspace removes the previous character relative to the cursor" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "hello");
    _ = try editor.handle(testing.allocator, .{ .kind = .left });
    const action = try editor.handle(testing.allocator, .{ .kind = .backspace });
    try testing.expectEqual(Action.redraw, action);
    try testing.expectEqualStrings("helo", editor.line());
    try testing.expectEqual(@as(usize, 3), editor.cursor());
}

test "delete removes the character under the cursor" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "hello");
    _ = try editor.handle(testing.allocator, .{ .kind = .left });
    _ = try editor.handle(testing.allocator, .{ .kind = .left });
    const action = try editor.handle(testing.allocator, .{ .kind = .delete });
    try testing.expectEqual(Action.redraw, action);
    try testing.expectEqualStrings("helo", editor.line());
    try testing.expectEqual(@as(usize, 3), editor.cursor());
}

test "left right home end move the cursor without mutating the buffer" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "hello");
    _ = try editor.handle(testing.allocator, .{ .kind = .left });
    _ = try editor.handle(testing.allocator, .{ .kind = .left });
    try testing.expectEqual(@as(usize, 3), editor.cursor());
    _ = try editor.handle(testing.allocator, .{ .kind = .home });
    try testing.expectEqual(@as(usize, 0), editor.cursor());
    _ = try editor.handle(testing.allocator, .{ .kind = .end });
    try testing.expectEqual(@as(usize, 5), editor.cursor());
    try testing.expectEqualStrings("hello", editor.line());
}

test "inserting in the middle keeps surrounding text" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "helo");
    _ = try editor.handle(testing.allocator, .{ .kind = .left });
    _ = try editor.handle(testing.allocator, .{ .kind = .left });
    _ = try editor.handle(testing.allocator, keyChar('l'));
    try testing.expectEqualStrings("hello", editor.line());
    try testing.expectEqual(@as(usize, 3), editor.cursor());
}

test "history up and down navigate submitted lines and restore the draft" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "first");
    try testing.expectEqual(Action.submit, try editor.handle(testing.allocator, .{ .kind = .enter }));
    editor.clear(testing.allocator);

    try typeString(&editor, testing.allocator, "second");
    try testing.expectEqual(Action.submit, try editor.handle(testing.allocator, .{ .kind = .enter }));
    editor.clear(testing.allocator);

    try typeString(&editor, testing.allocator, "draft");
    _ = try editor.handle(testing.allocator, .{ .kind = .up });
    try testing.expectEqualStrings("second", editor.line());
    _ = try editor.handle(testing.allocator, .{ .kind = .up });
    try testing.expectEqualStrings("first", editor.line());
    _ = try editor.handle(testing.allocator, .{ .kind = .down });
    try testing.expectEqualStrings("second", editor.line());
    _ = try editor.handle(testing.allocator, .{ .kind = .down });
    try testing.expectEqualStrings("draft", editor.line());
}

test "duplicate consecutive submissions are stored once in history" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "same");
    try testing.expectEqual(Action.submit, try editor.handle(testing.allocator, .{ .kind = .enter }));
    editor.clear(testing.allocator);
    try typeString(&editor, testing.allocator, "same");
    try testing.expectEqual(Action.submit, try editor.handle(testing.allocator, .{ .kind = .enter }));
    try testing.expectEqual(@as(usize, 1), editor.history.items.len);
}

test "enter returns submit without clearing the buffer" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "zigts_expert_meta");
    const action = try editor.handle(testing.allocator, .{ .kind = .enter });
    try testing.expectEqual(Action.submit, action);
    try testing.expectEqualStrings("zigts_expert_meta", editor.line());
}

test "clear resets the buffer and cursor" {
    var editor: LineEditor = .{};
    defer editor.deinit(testing.allocator);

    try typeString(&editor, testing.allocator, "abc");
    editor.clear(testing.allocator);
    try testing.expectEqualStrings("", editor.line());
    try testing.expectEqual(@as(usize, 0), editor.cursor());
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
