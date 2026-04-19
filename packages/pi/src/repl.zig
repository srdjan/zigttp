//! Line-buffered expert REPL driver.
//!
//! Natural language goes to the model by default. Deterministic slash commands,
//! safe raw `zigts ...` / `zig build ...` commands, and direct tool names are
//! routed locally.

const std = @import("std");
const registry_mod = @import("registry/registry.zig");
const agent = @import("agent.zig");

pub const Registry = registry_mod.Registry;
const ToolResult = registry_mod.ToolResult;

pub const DispatchOutcome = union(enum) {
    noop,
    quit,
    result: ToolResult,
};

const LocalCommand = struct {
    tool_name: []const u8,
    args: []const []const u8,
};

pub fn dispatchLine(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    line: []const u8,
) !DispatchOutcome {
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) return .noop;

    var tokens = try tokenizeLine(allocator, trimmed);
    defer tokens.deinit(allocator);
    const argv = tokens.items;
    if (argv.len == 0) return .noop;

    if (isQuit(argv[0])) return .quit;
    if (isHelp(argv[0])) return .{ .result = try renderHelp(allocator, registry) };

    if (parseSlashCommand(argv) orelse parseExplicitCommand(argv)) |cmd| {
        return .{ .result = try invokeTool(allocator, registry, cmd.tool_name, cmd.args) };
    }

    if (registry.findByName(argv[0])) |tool| {
        _ = tool;
        return .{ .result = try invokeTool(allocator, registry, argv[0], argv[1..]) };
    }

    const msg = try std.fmt.allocPrint(allocator, "unknown tool or command: {s}\n", .{argv[0]});
    return .{ .result = .{ .ok = false, .body = msg } };
}

pub fn shouldDispatchTool(registry: *const Registry, line: []const u8) bool {
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) return true;

    var first_token_iter = std.mem.tokenizeAny(u8, trimmed, " \t\r\n");
    const first = first_token_iter.next() orelse return true;
    if (isQuit(first) or isHelp(first)) return true;
    if (first.len > 0 and first[0] == '/') return true;
    if (std.mem.eql(u8, first, "zigts") or std.mem.eql(u8, first, "zig")) return true;
    return registry.findByName(first) != null;
}

fn invokeTool(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    name: []const u8,
    args: []const []const u8,
) !ToolResult {
    return registry.invoke(allocator, name, args) catch |err| switch (err) {
        registry_mod.RegistryError.ToolNotFound => registry_mod.ToolResult.errFmt(
            allocator,
            "unknown tool: {s}\n",
            .{name},
        ),
        else => registry_mod.ToolResult.errFmt(
            allocator,
            "tool {s} failed: {s}\n",
            .{ name, @errorName(err) },
        ),
    };
}

fn tokenizeLine(
    allocator: std.mem.Allocator,
    line: []const u8,
) !std.ArrayList([]const u8) {
    var tokens = std.ArrayList([]const u8).empty;
    errdefer tokens.deinit(allocator);
    var it = std.mem.tokenizeAny(u8, line, " \t\r\n");
    while (it.next()) |token| {
        try tokens.append(allocator, token);
    }
    return tokens;
}

fn parseSlashCommand(argv: []const []const u8) ?LocalCommand {
    if (argv.len == 0 or argv[0].len == 0 or argv[0][0] != '/') return null;
    if (std.mem.eql(u8, argv[0], "/meta")) return .{ .tool_name = "zigts_expert_meta", .args = &.{} };
    if (std.mem.eql(u8, argv[0], "/features")) return .{ .tool_name = "zigts_expert_features", .args = &.{} };
    if (std.mem.eql(u8, argv[0], "/modules")) return .{ .tool_name = "zigts_expert_modules", .args = &.{} };
    if (std.mem.eql(u8, argv[0], "/rule")) return .{ .tool_name = "zigts_expert_describe_rule", .args = argv[1..] };
    if (std.mem.eql(u8, argv[0], "/search")) return .{ .tool_name = "zigts_expert_search", .args = argv[1..] };
    if (std.mem.eql(u8, argv[0], "/verify")) return .{ .tool_name = "zigts_expert_verify_paths", .args = argv[1..] };
    if (std.mem.eql(u8, argv[0], "/check")) return .{ .tool_name = "zigts_check", .args = argv[1..] };
    if (std.mem.eql(u8, argv[0], "/build")) return .{ .tool_name = "zig_build_step", .args = argv[1..] };
    if (std.mem.eql(u8, argv[0], "/test")) return .{ .tool_name = "zig_test_step", .args = argv[1..] };
    return null;
}

fn parseExplicitCommand(argv: []const []const u8) ?LocalCommand {
    if (argv.len == 0) return null;
    if (std.mem.eql(u8, argv[0], "zigts")) {
        if (argv.len < 2) return null;
        if (std.mem.eql(u8, argv[1], "meta")) return .{ .tool_name = "zigts_expert_meta", .args = &.{} };
        if (std.mem.eql(u8, argv[1], "features")) return .{ .tool_name = "zigts_expert_features", .args = &.{} };
        if (std.mem.eql(u8, argv[1], "modules")) return .{ .tool_name = "zigts_expert_modules", .args = &.{} };
        if (std.mem.eql(u8, argv[1], "search")) return .{ .tool_name = "zigts_expert_search", .args = argv[2..] };
        if (std.mem.eql(u8, argv[1], "describe-rule")) return .{ .tool_name = "zigts_expert_describe_rule", .args = argv[2..] };
        if (std.mem.eql(u8, argv[1], "verify-paths")) return .{ .tool_name = "zigts_expert_verify_paths", .args = argv[2..] };
        if (std.mem.eql(u8, argv[1], "verify-modules")) return .{ .tool_name = "zigts_expert_verify_modules", .args = argv[2..] };
        if (std.mem.eql(u8, argv[1], "check")) return .{ .tool_name = "zigts_check", .args = argv[2..] };
        return null;
    }
    if (std.mem.eql(u8, argv[0], "zig") and argv.len >= 3 and std.mem.eql(u8, argv[1], "build")) {
        if (std.mem.eql(u8, argv[2], "test") or std.mem.startsWith(u8, argv[2], "test-")) {
            return .{ .tool_name = "zig_test_step", .args = argv[2..3] };
        }
        return .{ .tool_name = "zig_build_step", .args = argv[2..3] };
    }
    return null;
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

    try w.writeAll("Natural language is sent to the expert backend by default.\n");
    try w.writeAll("Explicit local commands:\n");
    try w.writeAll("  /meta  /features  /modules  /rule <code>  /search <term>\n");
    try w.writeAll("  /verify <path...>  /check <path>  /build <step>  /test [step]\n");
    try w.writeAll("  zigts meta|features|modules|search|describe-rule|verify-paths|verify-modules|check ...\n");
    try w.writeAll("  zig build <step>  zig build test[-...] \n\n");
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
        const banner = "zigts expert — NL by default, 'help' for commands, or 'quit'\n";
        _ = std.c.write(std.c.STDOUT_FILENO, banner.ptr, banner.len);
    }

    var session = try agent.initFromEnvWithRegistry(allocator, registry);
    defer session.deinit(allocator);

    var line_buf: [64 * 1024]u8 = undefined;
    while (true) {
        if (is_tty) {
            const prompt = "expert> ";
            _ = std.c.write(std.c.STDOUT_FILENO, prompt.ptr, prompt.len);
        }

        const maybe_line = try readLine(&line_buf);
        const line = maybe_line orelse break;

        const trimmed = std.mem.trim(u8, line, " \t\r\n");
        if (trimmed.len > 0 and !shouldDispatchTool(registry, trimmed)) {
            const rendered = agent.runOneTurn(allocator, &session, registry, trimmed, approveEdit) catch |err| {
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

fn approveEdit(file: []const u8) !bool {
    var prompt_buf: [512]u8 = undefined;
    const prompt = std.fmt.bufPrint(&prompt_buf, "Apply verified edit to {s}? [y/N] ", .{file}) catch "Apply verified edit? [y/N] ";
    _ = std.c.write(std.c.STDOUT_FILENO, prompt.ptr, prompt.len);

    var line_buf: [256]u8 = undefined;
    const maybe_line = try readLine(&line_buf);
    const line = maybe_line orelse return false;
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    if (trimmed.len == 0) return false;
    return trimmed[0] == 'y' or trimmed[0] == 'Y';
}

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

const testing = std.testing;
const meta_tool_mod = @import("tools/zigts_expert_meta.zig");
const check_tool_mod = @import("tools/zigts_check.zig");
const test_tool_mod = @import("tools/zig_test_step.zig");

fn buildMiniRegistry(allocator: std.mem.Allocator) !Registry {
    var reg: Registry = .{};
    errdefer reg.deinit(allocator);
    try reg.register(allocator, meta_tool_mod.tool);
    try reg.register(allocator, check_tool_mod.tool);
    try reg.register(allocator, test_tool_mod.tool);
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

test "help renders local command guidance" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "help");
    try expectResult(&outcome, testing.allocator, "/verify", true);
}

test "slash command routes locally" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "/meta");
    try expectResult(&outcome, testing.allocator, "\"compiler_version\"", true);
}

test "explicit zigts command routes locally" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "zigts meta");
    try expectResult(&outcome, testing.allocator, "\"policy_version\"", true);
}

test "raw tool name still dispatches directly" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    var outcome = try dispatchLine(testing.allocator, &reg, "zigts_expert_meta");
    try expectResult(&outcome, testing.allocator, "\"compiler_version\"", true);
}

test "shouldDispatchTool is false for plain natural language" {
    var reg = try buildMiniRegistry(testing.allocator);
    defer reg.deinit(testing.allocator);

    try testing.expect(!shouldDispatchTool(&reg, "add a GET route and then run tests"));
    try testing.expect(shouldDispatchTool(&reg, "/test"));
    try testing.expect(shouldDispatchTool(&reg, "zig build test-zigts"));
}
