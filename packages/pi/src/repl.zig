//! Line-buffered REPL driver for the in-process tool registry.
//!
//! The per-line logic lives in `dispatchLine` as a pure function so tests can
//! exercise parsing + lookup + invocation without touching stdin/stdout. `run`
//! wraps it in a minimal read/print loop intended for interactive poking and
//! phase-1 smoke tests, not for the eventual agent loop.
//!
//! Wire format: `<tool_name>[ <arg>]`. The first whitespace-delimited token is
//! the tool name; the remainder of the line (trimmed) is passed as a single
//! arg string. Tools that want more structure parse it themselves (e.g.
//! `zigts_expert_edit_simulate` expects a JSON blob in arg[0]). Meta commands
//! are `help`/`:h` (list tools) and `quit`/`exit`/`:q` (end the session).

const std = @import("std");
const registry_mod = @import("registry/registry.zig");
const agent = @import("agent.zig");

pub const Registry = registry_mod.Registry;
const ToolResult = registry_mod.ToolResult;
const ToolDef = registry_mod.ToolDef;

pub const DispatchOutcome = union(enum) {
    noop,
    quit,
    result: ToolResult,
};

pub fn dispatchLine(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    line: []const u8,
) !DispatchOutcome {
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) return .noop;

    const first_ws = std.mem.indexOfAny(u8, trimmed, " \t") orelse trimmed.len;
    const name = trimmed[0..first_ws];
    const rest = if (first_ws == trimmed.len)
        ""
    else
        std.mem.trim(u8, trimmed[first_ws..], " \t\r\n");

    if (isQuit(name)) return .quit;
    if (isHelp(name)) return .{ .result = try renderHelp(allocator, registry) };

    const args: []const []const u8 = if (rest.len == 0) &.{} else &.{rest};

    const result = registry.invoke(allocator, name, args) catch |err| switch (err) {
        registry_mod.RegistryError.ToolNotFound => {
            const msg = try std.fmt.allocPrint(allocator, "unknown tool: {s}\n", .{name});
            return .{ .result = .{ .ok = false, .body = msg } };
        },
        else => return err,
    };
    return .{ .result = result };
}

fn isQuit(name: []const u8) bool {
    return std.mem.eql(u8, name, "quit") or
        std.mem.eql(u8, name, "exit") or
        std.mem.eql(u8, name, ":q");
}

fn isHelp(name: []const u8) bool {
    return std.mem.eql(u8, name, "help") or std.mem.eql(u8, name, ":h");
}

fn renderHelp(allocator: std.mem.Allocator, registry: *const Registry) !ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("Registered tools:\n");
    for (registry.list()) |entry| {
        try w.print("  {s: <36}  {s}\n", .{ entry.name, entry.description });
    }
    try w.writeAll("\nMeta commands: help (:h), quit (:q)\n");

    buf = aw.toArrayList();
    return .{ .ok = true, .body = try buf.toOwnedSlice(allocator) };
}

pub fn run(
    allocator: std.mem.Allocator,
    registry: *const Registry,
) !void {
    const is_tty = std.c.isatty(std.c.STDIN_FILENO) != 0;
    if (is_tty) {
        const banner = "pi registry — type 'help' or 'quit'\n";
        _ = std.c.write(std.c.STDOUT_FILENO, banner.ptr, banner.len);
    }

    var session = agent.AgentSession.init();
    defer session.deinit(allocator);
    var agent_mode = false;

    var line_buf: [64 * 1024]u8 = undefined;
    while (true) {
        if (is_tty) {
            const prompt = "pi> ";
            _ = std.c.write(std.c.STDOUT_FILENO, prompt.ptr, prompt.len);
        }

        const maybe_line = try readLine(&line_buf);
        const line = maybe_line orelse break;

        const trimmed = std.mem.trim(u8, line, " \t\r\n");
        if (std.mem.eql(u8, trimmed, agent.toggle_command)) {
            agent_mode = !agent_mode;
            const msg = if (agent_mode) "agent mode: on\n" else "agent mode: off\n";
            _ = std.c.write(std.c.STDOUT_FILENO, msg.ptr, msg.len);
            continue;
        }

        if (agent_mode and trimmed.len > 0 and !isQuit(trimmed)) {
            const rendered = agent.runOneTurn(allocator, &session, registry, trimmed) catch |err| {
                var msg_buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "error: {s}\n", .{@errorName(err)}) catch "error\n";
                _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
                continue;
            };
            defer allocator.free(rendered);
            _ = std.c.write(std.c.STDOUT_FILENO, rendered.ptr, rendered.len);
            continue;
        }

        var outcome = dispatchLine(allocator, registry, line) catch |err| {
            var msg_buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&msg_buf, "error: {s}\n", .{@errorName(err)}) catch "error\n";
            _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            continue;
        };

        switch (outcome) {
            .noop => {},
            .quit => break,
            .result => |*result| {
                defer result.deinit(allocator);
                if (result.body.len > 0) {
                    _ = std.c.write(std.c.STDOUT_FILENO, result.body.ptr, result.body.len);
                    if (result.body[result.body.len - 1] != '\n') {
                        _ = std.c.write(std.c.STDOUT_FILENO, "\n", 1);
                    }
                }
            },
        }
    }
}

/// Read one line from stdin into `buf`. Returns the line (without the trailing
/// newline) or null on EOF-at-start. Lines longer than `buf` are truncated.
fn readLine(buf: []u8) !?[]const u8 {
    var len: usize = 0;
    while (len < buf.len) {
        var byte: [1]u8 = undefined;
        const n = std.posix.read(std.posix.STDIN_FILENO, &byte) catch |err| switch (err) {
            error.WouldBlock => continue,
            else => return err,
        };
        if (n == 0) {
            if (len == 0) return null;
            return buf[0..len];
        }
        if (byte[0] == '\n') return buf[0..len];
        buf[len] = byte[0];
        len += 1;
    }
    return buf[0..len];
}

// ---------------------------------------------------------------------------
// Tests
//
// The tests build a tiny real Registry holding the meta tool. Using a real
// tool (not a fake) keeps the test honest: it catches integration regressions
// between dispatchLine and Registry.invoke as well as parse-level bugs.
// ---------------------------------------------------------------------------

const testing = std.testing;
const meta_tool_mod = @import("tools/zigts_expert_meta.zig");

fn buildMiniRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);
    try reg.register(allocator, meta_tool_mod.tool);
    return reg;
}

fn expectResult(outcome: *DispatchOutcome, allocator: std.mem.Allocator, needle: []const u8, want_ok: bool) !void {
    switch (outcome.*) {
        .result => |*r| {
            defer r.deinit(allocator);
            try testing.expectEqual(want_ok, r.ok);
            try testing.expect(std.mem.indexOf(u8, r.body, needle) != null);
        },
        else => return error.TestFailed,
    }
}

test "blank line is a noop" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    const outcome = try dispatchLine(testing.allocator, &reg, "   ");
    try testing.expect(outcome == .noop);
}

test "quit and exit and :q all return .quit" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    for ([_][]const u8{ "quit", "exit", ":q", "  quit  " }) |line| {
        const outcome = try dispatchLine(testing.allocator, &reg, line);
        try testing.expect(outcome == .quit);
    }
}

test "help renders a tool listing" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "help");
    try expectResult(&outcome, testing.allocator, "zigts_expert_meta", true);
}

test "dispatchLine invokes the meta tool and emits v1 envelope" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "zigts_expert_meta");
    try expectResult(&outcome, testing.allocator, "\"compiler_version\":\"0.16.0\"", true);
}

test "unknown tool returns a not-ok result, not a Zig error" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "nope_not_a_tool");
    try expectResult(&outcome, testing.allocator, "unknown tool: nope_not_a_tool", false);
}

test "first-whitespace split passes the rest as a single arg" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "some_tool a b c");
    try expectResult(&outcome, testing.allocator, "unknown tool: some_tool", false);
}
