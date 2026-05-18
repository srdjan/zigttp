//! Pure layout math for the expert TUI: terminal probing, body/inspector
//! sizing, and composer cursor positioning. No app-state mutation.

const std = @import("std");

pub const prompt_label = "expert> ";

pub const LayoutMode = enum {
    split,
    stacked,
};

pub const TerminalSize = struct {
    width: usize,
    height: usize,
};

pub const Layout = struct {
    mode: LayoutMode,
    body_height: usize,
    feed_width: usize,
    inspector_width: usize,
    feed_height: usize,
    inspector_height: usize,

    pub fn totalBodyRows(self: Layout) usize {
        return switch (self.mode) {
            .split => self.body_height,
            .stacked => self.feed_height + self.inspector_height,
        };
    }
};

pub const ComposerView = struct {
    visible: []const u8,
    cursor_col: usize,
};

pub fn terminalSize() TerminalSize {
    var winsize: std.posix.winsize = .{
        .row = 0,
        .col = 0,
        .xpixel = 0,
        .ypixel = 0,
    };
    if (std.c.ioctl(std.c.STDOUT_FILENO, std.c.T.IOCGWINSZ, &winsize) == 0 and winsize.col > 0 and winsize.row > 0) {
        return .{
            .width = @intCast(winsize.col),
            .height = @intCast(winsize.row),
        };
    }
    return .{ .width = 80, .height = 25 };
}

pub fn computeLayout(size: TerminalSize) Layout {
    const composer_height: usize = 3;
    const status_height: usize = 1;
    const body_height = @max(@as(usize, 1), size.height -| (status_height + composer_height));

    if (size.width >= 100 and body_height >= 8) {
        const inspector_width = @min(@max(@as(usize, 32), size.width / 3), size.width -| 24);
        return .{
            .mode = .split,
            .body_height = body_height,
            .feed_width = size.width -| (inspector_width + 3),
            .inspector_width = inspector_width,
            .feed_height = body_height,
            .inspector_height = body_height,
        };
    }

    if (body_height <= 2) {
        return .{
            .mode = .stacked,
            .body_height = body_height,
            .feed_width = size.width,
            .inspector_width = size.width,
            .feed_height = body_height,
            .inspector_height = 0,
        };
    }

    var inspector_height = @max(@as(usize, 3), body_height / 3);
    if (inspector_height >= body_height) inspector_height = body_height -| 1;
    const feed_height = @max(@as(usize, 1), body_height -| inspector_height);
    return .{
        .mode = .stacked,
        .body_height = body_height,
        .feed_width = size.width,
        .inspector_width = size.width,
        .feed_height = feed_height,
        .inspector_height = @max(@as(usize, 1), body_height -| feed_height),
    };
}

pub fn visiblePrimaryRows(layout: Layout) usize {
    return switch (layout.mode) {
        .split => layout.body_height -| 1,
        .stacked => layout.feed_height -| 1,
    };
}

pub fn composerCursorPosition(
    layout: Layout,
    width: usize,
    line: []const u8,
    cursor: usize,
) struct { usize, usize } {
    const available = width -| prompt_label.len;
    const view = visibleComposer(line, cursor, available);
    const row = 1 + layout.totalBodyRows() + 2;
    const col = @min(width, prompt_label.len + view.cursor_col + 1);
    return .{ row, if (col == 0) 1 else col };
}

pub fn visibleComposer(line: []const u8, cursor: usize, max_width: usize) ComposerView {
    if (max_width == 0) return .{ .visible = "", .cursor_col = 0 };
    if (line.len <= max_width) return .{
        .visible = line,
        .cursor_col = @min(cursor, line.len),
    };

    const safe_cursor = @min(cursor, line.len);
    var start: usize = 0;
    if (safe_cursor >= max_width) {
        start = safe_cursor + 1 - max_width;
    }
    if (start + max_width > line.len) {
        start = line.len - max_width;
    }
    return .{
        .visible = line[start .. start + max_width],
        .cursor_col = safe_cursor - start,
    };
}
