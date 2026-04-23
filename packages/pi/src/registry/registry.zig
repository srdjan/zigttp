//! In-process tool registry.
//!
//! Append-only ToolDef list with unique-name insertion, lookup by name, and
//! model-facing JSON invocation alongside direct argv-style invocation.

const std = @import("std");
const tool_mod = @import("tool.zig");

pub const ToolDef = tool_mod.ToolDef;
pub const ToolResult = tool_mod.ToolResult;
pub const helpers = tool_mod;

pub const RegistryError = error{
    DuplicateTool,
    ToolNotFound,
};

pub const Registry = struct {
    entries: std.ArrayListUnmanaged(ToolDef) = .empty,

    pub fn deinit(self: *Registry, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
        self.* = .{};
    }

    pub fn register(
        self: *Registry,
        allocator: std.mem.Allocator,
        tool: ToolDef,
    ) (RegistryError || std.mem.Allocator.Error)!void {
        if (self.findByName(tool.name) != null) return RegistryError.DuplicateTool;
        try self.entries.append(allocator, tool);
    }

    pub fn findByName(self: *const Registry, name: []const u8) ?*const ToolDef {
        for (self.entries.items) |*entry| {
            if (std.mem.eql(u8, entry.name, name)) return entry;
        }
        return null;
    }

    pub fn count(self: *const Registry) usize {
        return self.entries.items.len;
    }

    pub fn list(self: *const Registry) []const ToolDef {
        return self.entries.items;
    }

    pub fn invoke(
        self: *const Registry,
        allocator: std.mem.Allocator,
        name: []const u8,
        args: []const []const u8,
    ) !ToolResult {
        const tool = self.findByName(name) orelse return RegistryError.ToolNotFound;
        return try tool.execute(allocator, args);
    }

    pub fn invokeJson(
        self: *const Registry,
        allocator: std.mem.Allocator,
        name: []const u8,
        args_json: []const u8,
    ) !ToolResult {
        const tool = self.findByName(name) orelse return RegistryError.ToolNotFound;
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const args = try tool.decode_json(arena.allocator(), args_json);
        return try tool.execute(allocator, args);
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

fn echoExecute(allocator: std.mem.Allocator, args: []const []const u8) anyerror!ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    for (args, 0..) |a, i| {
        if (i > 0) try aw.writer.writeByte(' ');
        try aw.writer.writeAll(a);
    }
    buf = aw.toArrayList();
    return .{ .ok = true, .llm_text = try buf.toOwnedSlice(allocator) };
}

const echo_tool: ToolDef = .{
    .name = "echo",
    .label = "Echo",
    .description = "Concatenate args with spaces",
    .input_schema =
    \\{"type":"object","properties":{"parts":{"type":"array","items":{"type":"string"}}},"required":["parts"]}
    ,
    .decode_json = decodeEchoJson,
    .execute = echoExecute,
};

fn decodeEchoJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return tool_mod.decodeStringArrayField(allocator, args_json, "parts");
}

test "register + findByName + invoke round-trip" {
    var reg: Registry = .{};
    defer reg.deinit(testing.allocator);

    try reg.register(testing.allocator, echo_tool);
    try testing.expectEqual(@as(usize, 1), reg.count());

    const found = reg.findByName("echo") orelse return error.TestFailed;
    try testing.expectEqualStrings("Echo", found.label);

    var result = try reg.invoke(testing.allocator, "echo", &.{ "hello", "world" });
    defer result.deinit(testing.allocator);
    try testing.expect(result.ok);
    try testing.expectEqualStrings("hello world", result.llm_text);
}

test "invokeJson decodes structured args before calling execute" {
    var reg: Registry = .{};
    defer reg.deinit(testing.allocator);

    try reg.register(testing.allocator, echo_tool);

    var result = try reg.invokeJson(testing.allocator, "echo", "{\"parts\":[\"hello\",\"json\"]}");
    defer result.deinit(testing.allocator);
    try testing.expect(result.ok);
    try testing.expectEqualStrings("hello json", result.llm_text);
}

test "duplicate registration fails" {
    var reg: Registry = .{};
    defer reg.deinit(testing.allocator);

    try reg.register(testing.allocator, echo_tool);
    const err = reg.register(testing.allocator, echo_tool);
    try testing.expectError(RegistryError.DuplicateTool, err);
}

test "unknown tool fails on findByName and invoke" {
    var reg: Registry = .{};
    defer reg.deinit(testing.allocator);

    try testing.expect(reg.findByName("nope") == null);
    const err = reg.invoke(testing.allocator, "nope", &.{});
    try testing.expectError(RegistryError.ToolNotFound, err);
}
