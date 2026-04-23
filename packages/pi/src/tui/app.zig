//! Raw-mode TUI event loop. Natural-language input goes to the expert
//! backend by default; direct tool invocations still route through the
//! same `repl.dispatchLine` path as the line-buffered REPL.
//!
//! The screen is split into normal scrollback above and a bottom-anchored
//! retained region (status line + input line) managed by `retained.zig`.
//! All scrollback writes are bracketed by `retained.beforeScrollback` /
//! `retained.afterScrollback` so the sticky rows survive every turn.

const std = @import("std");
const term = @import("term.zig");
const line_editor = @import("line_editor.zig");
const retained = @import("retained.zig");
const status_widget = @import("widgets/status_line.zig");
const theme_mod = @import("theme.zig");
const repl = @import("../repl.zig");
const agent = @import("../agent.zig");
const loop = @import("../loop.zig");
const app = @import("../app.zig");

const LineEditor = line_editor.LineEditor;
const KeyEvent = line_editor.KeyEvent;
const Registry = repl.Registry;
const ExpertFlags = app.ExpertFlags;

const prompt_label = "expert> ";

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
        .no_context_files = flags.no_context_files,
        .session_id = flags.session_id,
        .resume_latest = flags.resume_latest,
        .fork_session_id = flags.fork_session_id,
    });
    defer session.deinit(allocator);

    try writeBanner(&session);
    try redrawRetained(&session, editor.line());

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
                // Escape sequence: consume up to 2 more bytes (arrow / function
                // keys are typically ESC [ X) and ignore.
                i = @min(n, i + 3);
                continue;
            }

            const event = classifyByte(byte);
            i += 1;

            const action = try editor.handle(allocator, event);
            switch (action) {
                .none => {},
                .redraw => try redrawRetained(&session, editor.line()),
                .submit => {
                    const line_snapshot = editor.line();

                    // Scrollback area: the submitted line becomes history.
                    // Erase retained, echo the submitted line, redraw.
                    try emitScrollback(&session, "\r\n");

                    var outcome = repl.processSubmit(allocator, &session, registry, line_snapshot, approval_fn) catch |err| {
                        var msg_buf: [256]u8 = undefined;
                        const msg = std.fmt.bufPrint(&msg_buf, "error: {s}\r\n", .{@errorName(err)}) catch "error\r\n";
                        try emitScrollback(&session, msg);
                        editor.clear();
                        try redrawRetained(&session, editor.line());
                        continue;
                    };

                    switch (outcome) {
                        .noop => {},
                        .quit => return,
                        .rendered => |rendered| {
                            defer allocator.free(rendered);
                            try emitScrollbackBody(&session, rendered);
                        },
                        .tool_result => |*result| {
                            defer result.deinit(allocator);
                            try emitScrollbackBody(&session, result.body);
                        },
                        .session_resume => try agent.rebuildSession(allocator, &session, registry, repl.baseSessionConfig(flags, .{ .resume_latest = true })),
                        .session_new => try agent.rebuildSession(allocator, &session, registry, repl.baseSessionConfig(flags, .{})),
                        .session_compact => {
                            const msg = try agent.compact(allocator, &session);
                            defer allocator.free(msg);
                            try emitScrollbackBody(&session, msg);
                        },
                        .session_fork => {
                            const msg = try agent.fork(allocator, &session);
                            defer allocator.free(msg);
                            try emitScrollbackBody(&session, msg);
                        },
                    }

                    editor.clear();
                    try redrawRetained(&session, editor.line());
                },
                .cancel => {
                    try emitScrollback(&session, "\r\n");
                    return;
                },
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Retained region + scrollback plumbing
// ---------------------------------------------------------------------------

/// Single-threaded scratch buffer for the retained writer. The TUI loop
/// never recurses into itself so reusing the same buffer per frame is safe
/// and avoids a per-frame allocation.
var frame_buf: [4096]u8 = undefined;

fn retainedState(session: *const agent.AgentSession, input: []const u8) retained.State {
    return .{
        .status = .{
            .session_id = if (session.session_id) |s| s else null,
            .model = session.currentModel(),
            .tokens = session.token_totals,
        },
        .input = input,
        .prompt_label = prompt_label,
    };
}

fn redrawRetained(session: *const agent.AgentSession, input: []const u8) !void {
    var fw = std.Io.Writer.fixed(&frame_buf);
    // retained.redraw's max emission fits in 4 KiB for any realistic
    // terminal width; a larger input buffer would be truncated by the
    // writer-fixed wrap, which is still correct since the line will be
    // redrawn on the next keystroke.
    retained.redraw(&fw, session.theme, retainedState(session, input)) catch {};
    writeAll(fw.buffered());
}

/// Erase the retained region, print `body` as scrollback (CRLF-translated),
/// then re-anchor the retained region below the new scrollback bytes. All
/// three phases share `frame_buf`: the before and after writes compose
/// short ANSI escape sequences that always fit.
fn emitScrollbackBody(session: *agent.AgentSession, body: []const u8) !void {
    try flushRetainedCommand(retainedEraser);

    const stdout = StdoutAdapter{};
    try printBody(&stdout, body);

    try flushRetainedReanchor(session);
}

/// Short-circuit path: a fixed CRLF or short literal string that should
/// land above the retained region without extra CRLF translation.
fn emitScrollback(session: *agent.AgentSession, body: []const u8) !void {
    try flushRetainedCommand(retainedEraser);
    writeAll(body);
    try flushRetainedReanchor(session);
}

fn retainedEraser(w: *std.Io.Writer) !void {
    try retained.beforeScrollback(w);
}

/// Runs `emit` against a fixed writer over `frame_buf`, then flushes the
/// buffered bytes to stdout. Keeps the ceremony out of callers.
fn flushRetainedCommand(emit: *const fn (*std.Io.Writer) anyerror!void) !void {
    var fw = std.Io.Writer.fixed(&frame_buf);
    try emit(&fw);
    writeAll(fw.buffered());
}

fn flushRetainedReanchor(session: *const agent.AgentSession) !void {
    var fw = std.Io.Writer.fixed(&frame_buf);
    try retained.afterScrollback(&fw, session.theme, retainedState(session, ""));
    writeAll(fw.buffered());
}

fn writeBanner(_: *const agent.AgentSession) !void {
    const banner = "zigts expert - type a request, 'help' for tools, press Enter to submit, Ctrl-C or 'quit' to exit\r\n";
    writeAll(banner);
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
    if (s.len == 0) return;
    _ = std.c.write(std.c.STDOUT_FILENO, s.ptr, s.len);
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

const StdoutAdapter = struct {
    fn writeAll(_: *const StdoutAdapter, bytes: []const u8) error{}!void {
        _ = std.c.write(std.c.STDOUT_FILENO, bytes.ptr, bytes.len);
    }
};

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
// Tests (byte classification + rendering only; the event loop itself is
// I/O-driven and exercised by integration runs).
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

const transcript_mod = @import("../transcript.zig");
const turn = @import("../turn.zig");

fn renderAndPrint(allocator: std.mem.Allocator, entries: []const transcript_mod.OwnedEntry, out: *std.Io.Writer) !void {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    for (entries) |*entry| try transcript_mod.renderRich(&aw.writer, entry);

    buf = aw.toArrayList();
    try printBody(out, buf.items);
}

test "replay: canned entry stream renders deterministic CRLF output" {
    const allocator = testing.allocator;

    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(allocator);

    try tr.append(allocator, .{ .user_text = "add a GET route" });
    try tr.append(allocator, .{ .model_text = "Inspecting first." });
    const calls = [_]turn.ToolCall{
        .{ .id = "toolu_1", .name = "zigts_expert_meta", .args_json = "{}" },
    };
    try tr.append(allocator, .{ .assistant_tool_use = &calls });
    try tr.append(allocator, .{ .tool_result = .{
        .tool_use_id = "toolu_1",
        .tool_name = "zigts_expert_meta",
        .ok = true,
        .body = "{\"ok\":true}",
    } });
    try tr.append(allocator, .{ .proof_card = "contract ok" });
    try tr.append(allocator, .{ .diagnostic_box = "ZTS001 unsupported var" });

    var out_buf: std.ArrayList(u8) = .empty;
    defer out_buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &out_buf);

    try renderAndPrint(allocator, tr.entries.items, &aw.writer);

    out_buf = aw.toArrayList();
    const out = out_buf.items;

    var i: usize = 0;
    while (i < out.len) : (i += 1) {
        if (out[i] == '\n') {
            try testing.expect(i > 0 and out[i - 1] == '\r');
        }
    }

    try testing.expect(std.mem.indexOf(u8, out, "add a GET route") != null);
    try testing.expect(std.mem.indexOf(u8, out, "Inspecting first.") != null);
    try testing.expect(std.mem.indexOf(u8, out, "zigts_expert_meta") != null);
    try testing.expect(std.mem.indexOf(u8, out, "{\"ok\":true}") != null);
    try testing.expect(std.mem.indexOf(u8, out, "contract ok") != null);
    try testing.expect(std.mem.indexOf(u8, out, "ZTS001 unsupported var") != null);

    try testing.expect(std.mem.indexOf(u8, out, "proof") != null);
    try testing.expect(std.mem.indexOf(u8, out, "veto") != null);
}

test "replay: rendering the same stream twice is byte-identical" {
    const allocator = testing.allocator;

    var tr: transcript_mod.Transcript = .{};
    defer tr.deinit(allocator);

    try tr.append(allocator, .{ .user_text = "hello" });
    try tr.append(allocator, .{ .model_text = "world\nwith multiple\nlines" });
    try tr.append(allocator, .{ .proof_card = "ok" });

    var buf_a: std.ArrayList(u8) = .empty;
    defer buf_a.deinit(allocator);
    var aw_a: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf_a);
    try renderAndPrint(allocator, tr.entries.items, &aw_a.writer);
    buf_a = aw_a.toArrayList();

    var buf_b: std.ArrayList(u8) = .empty;
    defer buf_b.deinit(allocator);
    var aw_b: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf_b);
    try renderAndPrint(allocator, tr.entries.items, &aw_b.writer);
    buf_b = aw_b.toArrayList();

    try testing.expectEqualSlices(u8, buf_a.items, buf_b.items);
}

test "retainedState: mirrors session fields" {
    var session = agent.AgentSession.initStub();
    defer session.deinit(testing.allocator);

    const state = retainedState(&session, "typed");
    try testing.expect(state.status.model == null);
    try testing.expectEqualStrings("typed", state.input);
    try testing.expectEqualStrings("expert> ", state.prompt_label);
    try testing.expect(state.status.session_id == null);
}
