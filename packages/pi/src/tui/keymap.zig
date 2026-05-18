//! ANSI keypress decoding for the expert TUI raw-mode input loop.

const line_editor = @import("line_editor.zig");

const KeyEvent = line_editor.KeyEvent;
const KeyKind = line_editor.KeyKind;

pub fn parseKeyEvent(bytes: []const u8, start: usize) struct { KeyEvent, usize } {
    const b = bytes[start];
    if (b != 0x1b) return .{ classifySingleByte(b), 1 };
    if (start + 1 >= bytes.len) return .{ .{ .kind = .esc }, 1 };

    const second = bytes[start + 1];
    if (second == '[') {
        if (start + 2 >= bytes.len) return .{ .{ .kind = .ignore }, 2 };
        const third = bytes[start + 2];
        return switch (third) {
            'A' => .{ .{ .kind = .up }, 3 },
            'B' => .{ .{ .kind = .down }, 3 },
            'C' => .{ .{ .kind = .right }, 3 },
            'D' => .{ .{ .kind = .left }, 3 },
            'H' => .{ .{ .kind = .home }, 3 },
            'F' => .{ .{ .kind = .end }, 3 },
            'Z' => .{ .{ .kind = .shift_tab }, 3 },
            '1', '7', '4', '8', '3' => parseTildeTerminated(bytes, start, third),
            else => .{ .{ .kind = .ignore }, 3 },
        };
    }

    if (second == 'O') {
        if (start + 2 >= bytes.len) return .{ .{ .kind = .ignore }, 2 };
        return switch (bytes[start + 2]) {
            'H' => .{ .{ .kind = .home }, 3 },
            'F' => .{ .{ .kind = .end }, 3 },
            else => .{ .{ .kind = .ignore }, 3 },
        };
    }

    return .{ .{ .kind = .esc }, 1 };
}

pub fn parseTildeTerminated(bytes: []const u8, start: usize, code: u8) struct { KeyEvent, usize } {
    if (start + 3 >= bytes.len or bytes[start + 3] != '~') {
        return .{ .{ .kind = .ignore }, 3 };
    }
    const kind: KeyKind = switch (code) {
        '1', '7' => .home,
        '4', '8' => .end,
        '3' => .delete,
        else => .ignore,
    };
    return .{ .{ .kind = kind }, 4 };
}

pub fn classifySingleByte(b: u8) KeyEvent {
    return switch (b) {
        3 => .{ .kind = .ctrl_c },
        4 => .{ .kind = .eof },
        9 => .{ .kind = .tab },
        13, 10 => .{ .kind = .enter },
        127, 8 => .{ .kind = .backspace },
        32...126, 128...255 => .{ .kind = .char, .byte = b },
        else => .{ .kind = .ignore },
    };
}
