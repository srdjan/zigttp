//! Slash and explicit command parsing for the expert REPL.
//!
//! Keeps the command routing table in one place so repl.zig stays focused on
//! dispatch and rendering.

const std = @import("std");

pub const LocalCommand = struct {
    tool_name: []const u8,
    args: []const []const u8,
};

const CommandRow = struct {
    slash: ?[]const u8,
    explicit: ?[]const u8,
    tool: []const u8,
    takes_trailing_args: bool,
};

pub const command_table = [_]CommandRow{
    .{ .slash = "/meta", .explicit = "meta", .tool = "zigts_expert_meta", .takes_trailing_args = false },
    .{ .slash = "/features", .explicit = "features", .tool = "zigts_expert_features", .takes_trailing_args = false },
    .{ .slash = "/modules", .explicit = "modules", .tool = "zigts_expert_modules", .takes_trailing_args = false },
    .{ .slash = "/rule", .explicit = "describe-rule", .tool = "zigts_expert_describe_rule", .takes_trailing_args = true },
    .{ .slash = "/search", .explicit = "search", .tool = "zigts_expert_search", .takes_trailing_args = true },
    .{ .slash = "/verify", .explicit = "verify-paths", .tool = "zigts_expert_verify_paths", .takes_trailing_args = true },
    .{ .slash = null, .explicit = "verify-modules", .tool = "zigts_expert_verify_modules", .takes_trailing_args = true },
    .{ .slash = "/check", .explicit = "check", .tool = "zigts_check", .takes_trailing_args = true },
    .{ .slash = "/build", .explicit = null, .tool = "zig_build_step", .takes_trailing_args = true },
    .{ .slash = "/test", .explicit = null, .tool = "zig_test_step", .takes_trailing_args = true },
};

pub fn lookup(argv: []const []const u8) ?LocalCommand {
    if (argv.len == 0) return null;

    if (argv[0].len > 0 and argv[0][0] == '/') {
        inline for (command_table) |row| {
            if (row.slash) |slash| {
                if (std.mem.eql(u8, argv[0], slash)) {
                    const args: []const []const u8 = if (row.takes_trailing_args) argv[1..] else &.{};
                    return .{ .tool_name = row.tool, .args = args };
                }
            }
        }
        return null;
    }

    if (std.mem.eql(u8, argv[0], "zigts")) {
        if (argv.len < 2) return null;
        inline for (command_table) |row| {
            if (row.explicit) |name| {
                if (std.mem.eql(u8, argv[1], name)) {
                    const args: []const []const u8 = if (row.takes_trailing_args) argv[2..] else &.{};
                    return .{ .tool_name = row.tool, .args = args };
                }
            }
        }
        return null;
    }

    return parseZigBuild(argv);
}

fn parseZigBuild(argv: []const []const u8) ?LocalCommand {
    if (argv.len < 3) return null;
    if (!std.mem.eql(u8, argv[0], "zig")) return null;
    if (!std.mem.eql(u8, argv[1], "build")) return null;
    const step = argv[2];
    if (std.mem.eql(u8, step, "test") or std.mem.startsWith(u8, step, "test-")) {
        return .{ .tool_name = "zig_test_step", .args = argv[2..3] };
    }
    return .{ .tool_name = "zig_build_step", .args = argv[2..3] };
}

pub fn isQuit(name: []const u8) bool {
    return std.mem.eql(u8, name, "quit") or
        std.mem.eql(u8, name, "exit") or
        std.mem.eql(u8, name, ":q");
}

pub fn isHelp(name: []const u8) bool {
    return std.mem.eql(u8, name, "help") or std.mem.eql(u8, name, ":h");
}

pub const session_commands = [_][]const u8{ "/resume", "/new" };

pub fn isSessionResume(name: []const u8) bool {
    return std.mem.eql(u8, name, session_commands[0]);
}

pub fn isSessionNew(name: []const u8) bool {
    return std.mem.eql(u8, name, session_commands[1]);
}

const testing = std.testing;

test "lookup slash /meta returns meta tool" {
    const argv = [_][]const u8{"/meta"};
    const cmd = lookup(&argv) orelse return error.TestFailed;
    try testing.expectEqualStrings("zigts_expert_meta", cmd.tool_name);
    try testing.expectEqual(@as(usize, 0), cmd.args.len);
}

test "lookup explicit zigts meta returns meta tool" {
    const argv = [_][]const u8{ "zigts", "meta" };
    const cmd = lookup(&argv) orelse return error.TestFailed;
    try testing.expectEqualStrings("zigts_expert_meta", cmd.tool_name);
    try testing.expectEqual(@as(usize, 0), cmd.args.len);
}

test "lookup unknown slash returns null" {
    const argv = [_][]const u8{"/unknown"};
    try testing.expect(lookup(&argv) == null);
}

test "lookup zig build test-zigts returns zig_test_step" {
    const argv = [_][]const u8{ "zig", "build", "test-zigts" };
    const cmd = lookup(&argv) orelse return error.TestFailed;
    try testing.expectEqualStrings("zig_test_step", cmd.tool_name);
    try testing.expectEqual(@as(usize, 1), cmd.args.len);
    try testing.expectEqualStrings("test-zigts", cmd.args[0]);
}

test "lookup zig build my-step routes to zig_build_step" {
    const argv = [_][]const u8{ "zig", "build", "my-step" };
    const cmd = lookup(&argv) orelse return error.TestFailed;
    try testing.expectEqualStrings("zig_build_step", cmd.tool_name);
    try testing.expectEqualStrings("my-step", cmd.args[0]);
}

test "lookup /rule forwards trailing args" {
    const argv = [_][]const u8{ "/rule", "ZTS303" };
    const cmd = lookup(&argv) orelse return error.TestFailed;
    try testing.expectEqualStrings("zigts_expert_describe_rule", cmd.tool_name);
    try testing.expectEqual(@as(usize, 1), cmd.args.len);
    try testing.expectEqualStrings("ZTS303", cmd.args[0]);
}

test "isQuit and isHelp recognize aliases" {
    try testing.expect(isQuit("quit"));
    try testing.expect(isQuit("exit"));
    try testing.expect(isQuit(":q"));
    try testing.expect(!isQuit("help"));
    try testing.expect(isHelp("help"));
    try testing.expect(isHelp(":h"));
    try testing.expect(!isHelp("quit"));
}

test "isSessionResume and isSessionNew recognize slash commands" {
    try testing.expect(isSessionResume("/resume"));
    try testing.expect(!isSessionResume("/new"));
    try testing.expect(isSessionNew("/new"));
    try testing.expect(!isSessionNew("/resume"));
}
