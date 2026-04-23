const std = @import("std");
const json_writer = @import("providers/anthropic/json_writer.zig");

pub const DiagnosticItem = struct {
    code: []u8,
    severity: []u8,
    path: []u8,
    line: u32,
    column: u16,
    message: []u8,
    introduced_by_patch: ?bool = null,

    pub fn init(
        allocator: std.mem.Allocator,
        code: []const u8,
        severity: []const u8,
        path: []const u8,
        line: u32,
        column: u16,
        message: []const u8,
        introduced_by_patch: ?bool,
    ) !DiagnosticItem {
        return .{
            .code = try allocator.dupe(u8, code),
            .severity = try allocator.dupe(u8, severity),
            .path = try allocator.dupe(u8, path),
            .line = line,
            .column = column,
            .message = try allocator.dupe(u8, message),
            .introduced_by_patch = introduced_by_patch,
        };
    }

    pub fn clone(self: DiagnosticItem, allocator: std.mem.Allocator) !DiagnosticItem {
        return init(
            allocator,
            self.code,
            self.severity,
            self.path,
            self.line,
            self.column,
            self.message,
            self.introduced_by_patch,
        );
    }

    pub fn deinit(self: *DiagnosticItem, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
        allocator.free(self.severity);
        allocator.free(self.path);
        allocator.free(self.message);
        self.* = .{
            .code = &.{},
            .severity = &.{},
            .path = &.{},
            .line = 0,
            .column = 0,
            .message = &.{},
            .introduced_by_patch = null,
        };
    }
};

pub const DiagnosticsPayload = struct {
    summary: []u8,
    items: []DiagnosticItem,

    pub fn clone(self: DiagnosticsPayload, allocator: std.mem.Allocator) !DiagnosticsPayload {
        const owned_items = try allocator.alloc(DiagnosticItem, self.items.len);
        errdefer allocator.free(owned_items);
        for (owned_items) |*item| item.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                owned_items[i].deinit(allocator);
            }
            allocator.free(owned_items);
        }
        while (i < self.items.len) : (i += 1) {
            owned_items[i] = try self.items[i].clone(allocator);
        }
        return .{
            .summary = try allocator.dupe(u8, self.summary),
            .items = owned_items,
        };
    }

    pub fn deinit(self: *DiagnosticsPayload, allocator: std.mem.Allocator) void {
        allocator.free(self.summary);
        for (self.items) |*item| item.deinit(allocator);
        allocator.free(self.items);
        self.* = .{ .summary = &.{}, .items = &.{} };
    }
};

pub const ProofStats = struct {
    total: u32,
    new: u32,
    preexisting: ?u32 = null,
};

pub const ProofCardPayload = struct {
    title: []u8,
    summary: []u8,
    stats: ProofStats,
    highlights: [][]u8,

    pub fn clone(self: ProofCardPayload, allocator: std.mem.Allocator) !ProofCardPayload {
        const highlights = try allocator.alloc([]u8, self.highlights.len);
        errdefer allocator.free(highlights);
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                allocator.free(highlights[i]);
            }
            allocator.free(highlights);
        }
        while (i < self.highlights.len) : (i += 1) {
            highlights[i] = try allocator.dupe(u8, self.highlights[i]);
        }
        return .{
            .title = try allocator.dupe(u8, self.title),
            .summary = try allocator.dupe(u8, self.summary),
            .stats = self.stats,
            .highlights = highlights,
        };
    }

    pub fn deinit(self: *ProofCardPayload, allocator: std.mem.Allocator) void {
        allocator.free(self.title);
        allocator.free(self.summary);
        for (self.highlights) |highlight| allocator.free(highlight);
        allocator.free(self.highlights);
        self.* = .{
            .title = &.{},
            .summary = &.{},
            .stats = .{ .total = 0, .new = 0, .preexisting = null },
            .highlights = &.{},
        };
    }
};

pub const CommandOutcomePayload = struct {
    title: []u8,
    exit_code: ?u8,
    stdout: []u8,
    stderr: []u8,
    command: []u8,

    pub fn clone(self: CommandOutcomePayload, allocator: std.mem.Allocator) !CommandOutcomePayload {
        return .{
            .title = try allocator.dupe(u8, self.title),
            .exit_code = self.exit_code,
            .stdout = try allocator.dupe(u8, self.stdout),
            .stderr = try allocator.dupe(u8, self.stderr),
            .command = try allocator.dupe(u8, self.command),
        };
    }

    pub fn deinit(self: *CommandOutcomePayload, allocator: std.mem.Allocator) void {
        allocator.free(self.title);
        allocator.free(self.stdout);
        allocator.free(self.stderr);
        allocator.free(self.command);
        self.* = .{
            .title = &.{},
            .exit_code = null,
            .stdout = &.{},
            .stderr = &.{},
            .command = &.{},
        };
    }
};

pub const SessionTreeNode = struct {
    session_id: []u8,
    parent_id: ?[]u8,
    created_at_unix_ms: i64,
    depth: usize,
    is_current: bool,
    is_orphan_root: bool,

    pub fn clone(self: SessionTreeNode, allocator: std.mem.Allocator) !SessionTreeNode {
        return .{
            .session_id = try allocator.dupe(u8, self.session_id),
            .parent_id = if (self.parent_id) |parent_id|
                try allocator.dupe(u8, parent_id)
            else
                null,
            .created_at_unix_ms = self.created_at_unix_ms,
            .depth = self.depth,
            .is_current = self.is_current,
            .is_orphan_root = self.is_orphan_root,
        };
    }

    pub fn deinit(self: *SessionTreeNode, allocator: std.mem.Allocator) void {
        allocator.free(self.session_id);
        if (self.parent_id) |parent_id| allocator.free(parent_id);
        self.* = .{
            .session_id = &.{},
            .parent_id = null,
            .created_at_unix_ms = 0,
            .depth = 0,
            .is_current = false,
            .is_orphan_root = false,
        };
    }
};

pub const SessionTreePayload = struct {
    nodes: []SessionTreeNode,

    pub fn clone(self: SessionTreePayload, allocator: std.mem.Allocator) !SessionTreePayload {
        const owned = try allocator.alloc(SessionTreeNode, self.nodes.len);
        errdefer allocator.free(owned);
        for (owned) |*node| node.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                owned[i].deinit(allocator);
            }
            allocator.free(owned);
        }
        while (i < self.nodes.len) : (i += 1) {
            owned[i] = try self.nodes[i].clone(allocator);
        }
        return .{ .nodes = owned };
    }

    pub fn deinit(self: *SessionTreePayload, allocator: std.mem.Allocator) void {
        for (self.nodes) |*node| node.deinit(allocator);
        allocator.free(self.nodes);
        self.* = .{ .nodes = &.{} };
    }
};

pub const UiPayload = union(enum) {
    session_tree: SessionTreePayload,
    diagnostics: DiagnosticsPayload,
    proof_card: ProofCardPayload,
    command_outcome: CommandOutcomePayload,
    plain_text: []u8,

    pub fn clone(self: UiPayload, allocator: std.mem.Allocator) !UiPayload {
        return switch (self) {
            .session_tree => |payload| .{ .session_tree = try payload.clone(allocator) },
            .diagnostics => |payload| .{ .diagnostics = try payload.clone(allocator) },
            .proof_card => |payload| .{ .proof_card = try payload.clone(allocator) },
            .command_outcome => |payload| .{ .command_outcome = try payload.clone(allocator) },
            .plain_text => |text| .{ .plain_text = try allocator.dupe(u8, text) },
        };
    }

    pub fn deinit(self: *UiPayload, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .session_tree => |*payload| payload.deinit(allocator),
            .diagnostics => |*payload| payload.deinit(allocator),
            .proof_card => |*payload| payload.deinit(allocator),
            .command_outcome => |*payload| payload.deinit(allocator),
            .plain_text => |text| allocator.free(text),
        }
        self.* = .{ .plain_text = &.{} };
    }
};

pub fn writeJson(writer: *std.Io.Writer, payload: UiPayload) !void {
    try writer.writeByte('{');
    switch (payload) {
        .plain_text => |text| {
            try writer.writeAll("\"kind\":\"plain_text\",\"text\":");
            try json_writer.writeString(writer, text);
        },
        .session_tree => |tree| {
            try writer.writeAll("\"kind\":\"session_tree\",\"nodes\":[");
            for (tree.nodes, 0..) |node, i| {
                if (i > 0) try writer.writeByte(',');
                try writer.writeByte('{');
                try writer.writeAll("\"session_id\":");
                try json_writer.writeString(writer, node.session_id);
                try writer.writeAll(",\"parent_id\":");
                if (node.parent_id) |parent_id| {
                    try json_writer.writeString(writer, parent_id);
                } else {
                    try writer.writeAll("null");
                }
                try writer.writeAll(",\"created_at_unix_ms\":");
                try writer.print("{d}", .{node.created_at_unix_ms});
                try writer.writeAll(",\"depth\":");
                try writer.print("{d}", .{node.depth});
                try writer.writeAll(",\"is_current\":");
                try writer.writeAll(if (node.is_current) "true" else "false");
                try writer.writeAll(",\"is_orphan_root\":");
                try writer.writeAll(if (node.is_orphan_root) "true" else "false");
                try writer.writeByte('}');
            }
            try writer.writeByte(']');
        },
        .diagnostics => |diagnostics| {
            try writer.writeAll("\"kind\":\"diagnostics\",\"summary\":");
            try json_writer.writeString(writer, diagnostics.summary);
            try writer.writeAll(",\"items\":[");
            for (diagnostics.items, 0..) |item, i| {
                if (i > 0) try writer.writeByte(',');
                try writer.writeByte('{');
                try writer.writeAll("\"code\":");
                try json_writer.writeString(writer, item.code);
                try writer.writeAll(",\"severity\":");
                try json_writer.writeString(writer, item.severity);
                try writer.writeAll(",\"path\":");
                try json_writer.writeString(writer, item.path);
                try writer.writeAll(",\"line\":");
                try writer.print("{d}", .{item.line});
                try writer.writeAll(",\"column\":");
                try writer.print("{d}", .{item.column});
                try writer.writeAll(",\"message\":");
                try json_writer.writeString(writer, item.message);
                if (item.introduced_by_patch) |introduced_by_patch| {
                    try writer.writeAll(",\"introduced_by_patch\":");
                    try writer.writeAll(if (introduced_by_patch) "true" else "false");
                }
                try writer.writeByte('}');
            }
            try writer.writeByte(']');
        },
        .proof_card => |proof| {
            try writer.writeAll("\"kind\":\"proof_card\",\"title\":");
            try json_writer.writeString(writer, proof.title);
            try writer.writeAll(",\"summary\":");
            try json_writer.writeString(writer, proof.summary);
            try writer.writeAll(",\"stats\":{\"total\":");
            try writer.print("{d}", .{proof.stats.total});
            try writer.writeAll(",\"new\":");
            try writer.print("{d}", .{proof.stats.new});
            if (proof.stats.preexisting) |preexisting| {
                try writer.writeAll(",\"preexisting\":");
                try writer.print("{d}", .{preexisting});
            }
            try writer.writeAll("},\"highlights\":[");
            for (proof.highlights, 0..) |highlight, i| {
                if (i > 0) try writer.writeByte(',');
                try json_writer.writeString(writer, highlight);
            }
            try writer.writeByte(']');
        },
        .command_outcome => |command| {
            try writer.writeAll("\"kind\":\"command_outcome\",\"title\":");
            try json_writer.writeString(writer, command.title);
            try writer.writeAll(",\"exit_code\":");
            if (command.exit_code) |exit_code| {
                try writer.print("{d}", .{exit_code});
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(",\"stdout\":");
            try json_writer.writeString(writer, command.stdout);
            try writer.writeAll(",\"stderr\":");
            try json_writer.writeString(writer, command.stderr);
            try writer.writeAll(",\"command\":");
            try json_writer.writeString(writer, command.command);
        },
    }
    try writer.writeByte('}');
}

pub fn parse(allocator: std.mem.Allocator, value: std.json.Value) !UiPayload {
    if (value != .object) return error.InvalidUiPayload;
    const obj = value.object;
    const kind_val = obj.get("kind") orelse return error.InvalidUiPayload;
    if (kind_val != .string) return error.InvalidUiPayload;

    if (std.mem.eql(u8, kind_val.string, "plain_text")) {
        const text_val = obj.get("text") orelse return error.InvalidUiPayload;
        if (text_val != .string) return error.InvalidUiPayload;
        return .{ .plain_text = try allocator.dupe(u8, text_val.string) };
    }
    if (std.mem.eql(u8, kind_val.string, "session_tree")) {
        const nodes_val = obj.get("nodes") orelse return error.InvalidUiPayload;
        if (nodes_val != .array) return error.InvalidUiPayload;
        const nodes = try allocator.alloc(SessionTreeNode, nodes_val.array.items.len);
        errdefer allocator.free(nodes);
        for (nodes) |*node| node.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                nodes[i].deinit(allocator);
            }
            allocator.free(nodes);
        }
        while (i < nodes_val.array.items.len) : (i += 1) {
            const item = nodes_val.array.items[i];
            if (item != .object) return error.InvalidUiPayload;
            const item_obj = item.object;
            const session_id = getString(item_obj, "session_id") orelse return error.InvalidUiPayload;
            const parent_id = getOptionalString(item_obj, "parent_id") catch return error.InvalidUiPayload;
            const created_at_unix_ms = getInteger(item_obj, "created_at_unix_ms") orelse return error.InvalidUiPayload;
            const depth = getUnsigned(item_obj, "depth") orelse return error.InvalidUiPayload;
            const is_current = getBool(item_obj, "is_current") orelse return error.InvalidUiPayload;
            const is_orphan_root = getBool(item_obj, "is_orphan_root") orelse return error.InvalidUiPayload;
            nodes[i] = .{
                .session_id = try allocator.dupe(u8, session_id),
                .parent_id = if (parent_id) |pid| try allocator.dupe(u8, pid) else null,
                .created_at_unix_ms = created_at_unix_ms,
                .depth = depth,
                .is_current = is_current,
                .is_orphan_root = is_orphan_root,
            };
        }
        return .{ .session_tree = .{ .nodes = nodes } };
    }
    if (std.mem.eql(u8, kind_val.string, "diagnostics")) {
        const summary = getString(obj, "summary") orelse return error.InvalidUiPayload;
        const items_val = obj.get("items") orelse return error.InvalidUiPayload;
        if (items_val != .array) return error.InvalidUiPayload;
        const items = try allocator.alloc(DiagnosticItem, items_val.array.items.len);
        errdefer allocator.free(items);
        for (items) |*item| item.* = undefined;
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                items[i].deinit(allocator);
            }
            allocator.free(items);
        }
        while (i < items_val.array.items.len) : (i += 1) {
            const item_val = items_val.array.items[i];
            if (item_val != .object) return error.InvalidUiPayload;
            const item_obj = item_val.object;
            items[i] = try DiagnosticItem.init(
                allocator,
                getString(item_obj, "code") orelse return error.InvalidUiPayload,
                getString(item_obj, "severity") orelse return error.InvalidUiPayload,
                getString(item_obj, "path") orelse return error.InvalidUiPayload,
                @intCast(getUnsigned(item_obj, "line") orelse return error.InvalidUiPayload),
                @intCast(getUnsigned(item_obj, "column") orelse return error.InvalidUiPayload),
                getString(item_obj, "message") orelse return error.InvalidUiPayload,
                getBool(item_obj, "introduced_by_patch"),
            );
        }
        return .{ .diagnostics = .{
            .summary = try allocator.dupe(u8, summary),
            .items = items,
        } };
    }
    if (std.mem.eql(u8, kind_val.string, "proof_card")) {
        const title = getString(obj, "title") orelse return error.InvalidUiPayload;
        const summary = getString(obj, "summary") orelse return error.InvalidUiPayload;
        const stats_val = obj.get("stats") orelse return error.InvalidUiPayload;
        if (stats_val != .object) return error.InvalidUiPayload;
        const stats_obj = stats_val.object;
        const highlights_val = obj.get("highlights") orelse return error.InvalidUiPayload;
        if (highlights_val != .array) return error.InvalidUiPayload;
        const highlights = try allocator.alloc([]u8, highlights_val.array.items.len);
        errdefer allocator.free(highlights);
        var i: usize = 0;
        errdefer {
            while (i > 0) {
                i -= 1;
                allocator.free(highlights[i]);
            }
            allocator.free(highlights);
        }
        while (i < highlights_val.array.items.len) : (i += 1) {
            const highlight = highlights_val.array.items[i];
            if (highlight != .string) return error.InvalidUiPayload;
            highlights[i] = try allocator.dupe(u8, highlight.string);
        }
        return .{ .proof_card = .{
            .title = try allocator.dupe(u8, title),
            .summary = try allocator.dupe(u8, summary),
            .stats = .{
                .total = @intCast(getUnsigned(stats_obj, "total") orelse return error.InvalidUiPayload),
                .new = @intCast(getUnsigned(stats_obj, "new") orelse return error.InvalidUiPayload),
                .preexisting = if (getUnsigned(stats_obj, "preexisting")) |preexisting|
                    @intCast(preexisting)
                else
                    null,
            },
            .highlights = highlights,
        } };
    }
    if (std.mem.eql(u8, kind_val.string, "command_outcome")) {
        const title = getString(obj, "title") orelse return error.InvalidUiPayload;
        const stdout = getString(obj, "stdout") orelse return error.InvalidUiPayload;
        const stderr = getString(obj, "stderr") orelse return error.InvalidUiPayload;
        const command = getString(obj, "command") orelse return error.InvalidUiPayload;
        return .{ .command_outcome = .{
            .title = try allocator.dupe(u8, title),
            .exit_code = if (getUnsigned(obj, "exit_code")) |exit_code|
                @intCast(exit_code)
            else
                null,
            .stdout = try allocator.dupe(u8, stdout),
            .stderr = try allocator.dupe(u8, stderr),
            .command = try allocator.dupe(u8, command),
        } };
    }

    return error.InvalidUiPayload;
}

fn getString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    return if (value == .string) value.string else null;
}

fn getOptionalString(obj: std.json.ObjectMap, key: []const u8) !?[]const u8 {
    const value = obj.get(key) orelse return null;
    return switch (value) {
        .null => null,
        .string => value.string,
        else => error.InvalidUiPayload,
    };
}

fn getBool(obj: std.json.ObjectMap, key: []const u8) ?bool {
    const value = obj.get(key) orelse return null;
    return if (value == .bool) value.bool else null;
}

fn getInteger(obj: std.json.ObjectMap, key: []const u8) ?i64 {
    const value = obj.get(key) orelse return null;
    return if (value == .integer) value.integer else null;
}

fn getUnsigned(obj: std.json.ObjectMap, key: []const u8) ?usize {
    const value = obj.get(key) orelse return null;
    if (value != .integer or value.integer < 0) return null;
    return std.math.cast(usize, value.integer);
}

const testing = std.testing;

fn roundTrip(allocator: std.mem.Allocator, payload: UiPayload) !UiPayload {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeJson(&aw.writer, payload);
    buf = aw.toArrayList();

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, buf.items, .{});
    defer parsed.deinit();
    return try parse(allocator, parsed.value);
}

test "plain_text payload round-trips" {
    var payload: UiPayload = .{ .plain_text = try testing.allocator.dupe(u8, "hello") };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .plain_text => |text| try testing.expectEqualStrings("hello", text),
        else => return error.TestFailed,
    }
}

test "diagnostics payload round-trips" {
    const items = try testing.allocator.alloc(DiagnosticItem, 1);
    items[0] = try DiagnosticItem.init(
        testing.allocator,
        "ZTS001",
        "error",
        "handler.ts",
        3,
        7,
        "unsupported feature",
        true,
    );
    var payload: UiPayload = .{ .diagnostics = .{
        .summary = try testing.allocator.dupe(u8, "1 violation"),
        .items = items,
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .diagnostics => |diagnostics| {
            try testing.expectEqualStrings("1 violation", diagnostics.summary);
            try testing.expectEqual(@as(usize, 1), diagnostics.items.len);
            try testing.expectEqualStrings("ZTS001", diagnostics.items[0].code);
            try testing.expect(diagnostics.items[0].introduced_by_patch.?);
        },
        else => return error.TestFailed,
    }
}

test "proof card payload round-trips" {
    const highlights = try testing.allocator.alloc([]u8, 2);
    highlights[0] = try testing.allocator.dupe(u8, "retry_safe");
    highlights[1] = try testing.allocator.dupe(u8, "idempotent");
    var payload: UiPayload = .{ .proof_card = .{
        .title = try testing.allocator.dupe(u8, "Compiler verification"),
        .summary = try testing.allocator.dupe(u8, "No new violations"),
        .stats = .{ .total = 1, .new = 0, .preexisting = 1 },
        .highlights = highlights,
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .proof_card => |proof| {
            try testing.expectEqualStrings("Compiler verification", proof.title);
            try testing.expectEqual(@as(u32, 1), proof.stats.total);
            try testing.expectEqual(@as(usize, 2), proof.highlights.len);
        },
        else => return error.TestFailed,
    }
}

test "command outcome payload round-trips" {
    var payload: UiPayload = .{ .command_outcome = .{
        .title = try testing.allocator.dupe(u8, "zig test"),
        .exit_code = 0,
        .stdout = try testing.allocator.dupe(u8, "ok"),
        .stderr = try testing.allocator.dupe(u8, ""),
        .command = try testing.allocator.dupe(u8, "zig build test-zigts"),
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .command_outcome => |command| {
            try testing.expectEqualStrings("zig test", command.title);
            try testing.expectEqual(@as(?u8, 0), command.exit_code);
            try testing.expectEqualStrings("zig build test-zigts", command.command);
        },
        else => return error.TestFailed,
    }
}

test "session tree payload round-trips" {
    const nodes = try testing.allocator.alloc(SessionTreeNode, 2);
    nodes[0] = .{
        .session_id = try testing.allocator.dupe(u8, "root"),
        .parent_id = null,
        .created_at_unix_ms = 1,
        .depth = 0,
        .is_current = false,
        .is_orphan_root = false,
    };
    nodes[1] = .{
        .session_id = try testing.allocator.dupe(u8, "child"),
        .parent_id = try testing.allocator.dupe(u8, "root"),
        .created_at_unix_ms = 2,
        .depth = 1,
        .is_current = true,
        .is_orphan_root = false,
    };
    var payload: UiPayload = .{ .session_tree = .{ .nodes = nodes } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .session_tree => |tree| {
            try testing.expectEqual(@as(usize, 2), tree.nodes.len);
            try testing.expectEqualStrings("child", tree.nodes[1].session_id);
            try testing.expect(tree.nodes[1].is_current);
        },
        else => return error.TestFailed,
    }
}
