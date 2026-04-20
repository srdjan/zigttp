//! Raw-mode TUI event loop. Natural-language input goes to the expert
//! backend by default; direct tool invocations still route through the
//! same `repl.dispatchLine` path as the line-buffered REPL.
//!
//! The loop reads bytes from stdin, classifies each into a `KeyEvent`, feeds
//! it to the `LineEditor`, and on submit hands the collected line to the
//! registry. Tool output is routed through `printBody` which translates `\n`
//! to `\r\n` because raw mode disables OPOST.

const std = @import("std");
const term = @import("term.zig");
const line_editor = @import("line_editor.zig");
const repl = @import("../repl.zig");
const agent = @import("../agent.zig");
const loop = @import("../loop.zig");
const app = @import("../app.zig");

const LineEditor = line_editor.LineEditor;
const KeyEvent = line_editor.KeyEvent;
const Registry = repl.Registry;
const ExpertFlags = app.ExpertFlags;

// The redraw prefix is built at comptime so one syscall covers both the
// clear-line escape and the label instead of two per keystroke.
const prompt_prefix = "\r\x1b[2K" ++ "expert> ";

pub fn run(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    flags: ExpertFlags,
    policy: loop.ApprovalPolicy,
) !void {
    const approval_fn = selectApprovalFn(policy);

    var raw = try term.RawMode.enter();
    defer raw.exit();

    var editor: LineEditor = .{};
    defer editor.deinit(allocator);

    var session = try agent.initFromEnvWithSessionConfig(allocator, registry, .{
        .no_session = flags.no_session,
        .no_persist_tool_output = flags.no_persist_tool_output,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
    });
    defer session.deinit(allocator);

    const banner = "zigts expert - type a request, 'help' for tools, press Enter to submit, Ctrl-C or 'quit' to exit\r\n";
    writeAll(banner);
    redrawPrompt(editor.line());

    var input_buf: [256]u8 = undefined;

    while (true) {
        const n = std.posix.read(std.posix.STDIN_FILENO, &input_buf) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) break;

        var i: usize = 0;
        while (i < n) {
            const byte = input_buf[i];
            if (byte == 0x1b) {
                // Escape sequence - consume up to 2 more bytes if present
                // (typical arrow/function key shape is ESC [ X) and ignore.
                i = @min(n, i + 3);
                continue;
            }

            const event = classifyByte(byte);
            i += 1;

            const action = try editor.handle(allocator, event);
            switch (action) {
                .none => {},
                .redraw => redrawPrompt(editor.line()),
                .submit => {
                    const line_snapshot = editor.line();
                    writeAll("\r\n");

                    if (std.mem.eql(u8, line_snapshot, "quit") or
                        std.mem.eql(u8, line_snapshot, "exit") or
                        std.mem.eql(u8, line_snapshot, ":q"))
                    {
                        return;
                    }

                    if (line_snapshot.len > 0 and !repl.shouldDispatchTool(registry, line_snapshot)) {
                        const rendered = agent.runOneTurn(allocator, &session, registry, line_snapshot, approval_fn) catch |err| {
                            var msg_buf: [256]u8 = undefined;
                            const msg = std.fmt.bufPrint(&msg_buf, "error: {s}\r\n", .{@errorName(err)}) catch "error\r\n";
                            writeAll(msg);
                            editor.clear();
                            redrawPrompt(editor.line());
                            continue;
                        };
                        defer allocator.free(rendered);
                        const stdout = StdoutAdapter{};
                        try printBody(&stdout, rendered);
                        editor.clear();
                        redrawPrompt(editor.line());
                        continue;
                    }

                    var outcome = repl.dispatchLine(allocator, registry, line_snapshot) catch |err| {
                        var msg_buf: [256]u8 = undefined;
                        const msg = std.fmt.bufPrint(&msg_buf, "error: {s}\r\n", .{@errorName(err)}) catch "error\r\n";
                        writeAll(msg);
                        editor.clear();
                        redrawPrompt(editor.line());
                        continue;
                    };

                    switch (outcome) {
                        .noop => {},
                        .quit => return,
                        .result => |*result| {
                            defer result.deinit(allocator);
                            const stdout = StdoutAdapter{};
                            try printBody(&stdout, result.body);
                        },
                        .session_resume => try agent.rebuildSession(allocator, &session, registry, flags.no_session, flags.no_persist_tool_output, true),
                        .session_new => try agent.rebuildSession(allocator, &session, registry, flags.no_session, flags.no_persist_tool_output, false),
                    }

                    editor.clear();
                    redrawPrompt(editor.line());
                },
                .cancel => {
                    writeAll("\r\n");
                    return;
                },
            }
        }
    }
}

fn classifyByte(b: u8) KeyEvent {
    return switch (b) {
        3 => .{ .kind = .ctrl_c },
        4 => .{ .kind = .eof },
        13, 10 => .{ .kind = .enter },
        127, 8 => .{ .kind = .backspace },
        32...126, 128...255 => .{ .kind = .char, .byte = b },
        else => .{ .kind = .ignore },
    };
}

fn writeAll(s: []const u8) void {
    _ = std.c.write(std.c.STDOUT_FILENO, s.ptr, s.len);
}

fn redrawPrompt(current: []const u8) void {
    writeAll(prompt_prefix);
    if (current.len > 0) writeAll(current);
}

fn approveEdit(file: []const u8) !bool {
    var prompt_buf: [512]u8 = undefined;
    const prompt = std.fmt.bufPrint(&prompt_buf, "\r\nApply verified edit to {s}? [y/N] ", .{file}) catch "\r\nApply verified edit? [y/N] ";
    writeAll(prompt);

    while (true) {
        var byte: [1]u8 = undefined;
        const n = std.posix.read(std.posix.STDIN_FILENO, &byte) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) {
            writeAll("\r\n");
            return false;
        }
        switch (byte[0]) {
            'y', 'Y' => {
                writeAll("\r\n");
                return true;
            },
            'n', 'N', 3, 4, 13, 10 => {
                writeAll("\r\n");
                return false;
            },
            else => {},
        }
    }
}

/// Writer adapter that forwards straight to stdout via std.c.write. Used by
/// the `run()` loop to keep `printBody` non-allocating in production while
/// letting tests pass a buffered writer instead.
const StdoutAdapter = struct {
    fn writeAll(_: *const StdoutAdapter, bytes: []const u8) error{}!void {
        _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
    }
};

/// Translate LF to CRLF because raw mode has OPOST off, and always terminate
/// with CRLF so the next prompt draws on a fresh line even when the body
/// itself ended mid-line. Takes a writer so tests can pin the exact output;
/// the `run()` loop passes a `StdoutAdapter`.
fn printBody(writer: anytype, body: []const u8) !void {
    var start: usize = 0;
    var i: usize = 0;
    while (i < body.len) : (i += 1) {
        if (body[i] == '\n') {
            if (i > start) try writer.writeAll(body[start..i]);
            try writer.writeAll("\r\n");
            start = i + 1;
        }
    }
    if (start < body.len) {
        try writer.writeAll(body[start..]);
        try writer.writeAll("\r\n");
    }
}

fn selectApprovalFn(policy: loop.ApprovalPolicy) loop.ApprovalFn {
    return loop.resolveApprovalFn(policy, approveEdit);
}


// ---------------------------------------------------------------------------
// Tests (byte classification only; the event loop itself is I/O-driven)
// ---------------------------------------------------------------------------

const testing = std.testing;

test "classifyByte maps control bytes to the right kinds" {
    try testing.expectEqual(line_editor.KeyKind.ctrl_c, classifyByte(3).kind);
    try testing.expectEqual(line_editor.KeyKind.eof, classifyByte(4).kind);
    try testing.expectEqual(line_editor.KeyKind.enter, classifyByte(13).kind);
    try testing.expectEqual(line_editor.KeyKind.enter, classifyByte(10).kind);
    try testing.expectEqual(line_editor.KeyKind.backspace, classifyByte(127).kind);
    try testing.expectEqual(line_editor.KeyKind.backspace, classifyByte(8).kind);
}

test "classifyByte: printable ASCII becomes char" {
    const e = classifyByte('a');
    try testing.expectEqual(line_editor.KeyKind.char, e.kind);
    try testing.expectEqual(@as(u8, 'a'), e.byte);
}

test "classifyByte: high-bit bytes become char (UTF-8 passthrough)" {
    const e = classifyByte(0xc3);
    try testing.expectEqual(line_editor.KeyKind.char, e.kind);
    try testing.expectEqual(@as(u8, 0xc3), e.byte);
}

test "classifyByte: low control bytes fall through to ignore" {
    try testing.expectEqual(line_editor.KeyKind.ignore, classifyByte(0).kind);
    try testing.expectEqual(line_editor.KeyKind.ignore, classifyByte(1).kind);
    try testing.expectEqual(line_editor.KeyKind.ignore, classifyByte(31).kind);
}

fn captureBody(body: []const u8) ![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(testing.allocator, &buf);
    try printBody(&aw.writer, body);
    buf = aw.toArrayList();
    return try buf.toOwnedSlice(testing.allocator);
}

test "printBody: newline-terminated body translates LF to CRLF with no extra tail" {
    const out = try captureBody("hello\n");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("hello\r\n", out);
}

test "printBody: mid-line body gets a trailing CRLF" {
    const out = try captureBody("hello");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("hello\r\n", out);
}

test "printBody: multi-line body translates every LF" {
    const out = try captureBody("hello\nworld\n");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("hello\r\nworld\r\n", out);
}

test "printBody: multi-line body with unterminated tail still gets one trailing CRLF" {
    const out = try captureBody("hello\nworld");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("hello\r\nworld\r\n", out);
}

test "printBody: double newline passes through as two CRLFs" {
    const out = try captureBody("\n\n");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("\r\n\r\n", out);
}

test "printBody: empty body writes nothing" {
    const out = try captureBody("");
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("", out);
}

test "selectApprovalFn: auto_approve resolves to loop.autoApprove" {
    try testing.expect(selectApprovalFn(.auto_approve) == loop.autoApprove);
}

test "selectApprovalFn: auto_reject resolves to loop.autoReject" {
    try testing.expect(selectApprovalFn(.auto_reject) == loop.autoReject);
}

test "selectApprovalFn: ask resolves to the interactive approveEdit" {
    try testing.expect(selectApprovalFn(.ask) == approveEdit);
}
