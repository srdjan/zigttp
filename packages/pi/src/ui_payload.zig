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

pub const PropertiesSnapshot = struct {
    pure: bool,
    read_only: bool,
    deterministic: bool,
    retry_safe: bool,
    idempotent: bool,
    state_isolated: bool,
    injection_safe: bool,
    fault_covered: bool,
};

pub const VerifiedPatchPayload = struct {
    file: []u8,
    policy_hash: []u8,
    stats: ProofStats,
    before: ?[]u8,
    after: []u8,
    after_properties: ?PropertiesSnapshot,
    post_apply_ok: bool,
    post_apply_summary: ?[]u8,

    pub fn clone(self: VerifiedPatchPayload, allocator: std.mem.Allocator) !VerifiedPatchPayload {
        const file_copy = try allocator.dupe(u8, self.file);
        errdefer allocator.free(file_copy);
        const policy_copy = try allocator.dupe(u8, self.policy_hash);
        errdefer allocator.free(policy_copy);
        const before_copy: ?[]u8 = if (self.before) |b| try allocator.dupe(u8, b) else null;
        errdefer if (before_copy) |b| allocator.free(b);
        const after_copy = try allocator.dupe(u8, self.after);
        errdefer allocator.free(after_copy);
        const post_apply_summary_copy: ?[]u8 = if (self.post_apply_summary) |s|
            try allocator.dupe(u8, s)
        else
            null;
        errdefer if (post_apply_summary_copy) |s| allocator.free(s);
        return .{
            .file = file_copy,
            .policy_hash = policy_copy,
            .stats = self.stats,
            .before = before_copy,
            .after = after_copy,
            .after_properties = self.after_properties,
            .post_apply_ok = self.post_apply_ok,
            .post_apply_summary = post_apply_summary_copy,
        };
    }

    pub fn deinit(self: *VerifiedPatchPayload, allocator: std.mem.Allocator) void {
        allocator.free(self.file);
        allocator.free(self.policy_hash);
        if (self.before) |b| allocator.free(b);
        allocator.free(self.after);
        if (self.post_apply_summary) |s| allocator.free(s);
        self.* = .{
            .file = &.{},
            .policy_hash = &.{},
            .stats = .{ .total = 0, .new = 0, .preexisting = null },
            .before = null,
            .after = &.{},
            .after_properties = null,
            .post_apply_ok = false,
            .post_apply_summary = null,
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
    verified_patch: VerifiedPatchPayload,
    plain_text: []u8,

    pub fn clone(self: UiPayload, allocator: std.mem.Allocator) !UiPayload {
        return switch (self) {
            .session_tree => |payload| .{ .session_tree = try payload.clone(allocator) },
            .diagnostics => |payload| .{ .diagnostics = try payload.clone(allocator) },
            .proof_card => |payload| .{ .proof_card = try payload.clone(allocator) },
            .command_outcome => |payload| .{ .command_outcome = try payload.clone(allocator) },
            .verified_patch => |payload| .{ .verified_patch = try payload.clone(allocator) },
            .plain_text => |text| .{ .plain_text = try allocator.dupe(u8, text) },
        };
    }

    pub fn deinit(self: *UiPayload, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .session_tree => |*payload| payload.deinit(allocator),
            .diagnostics => |*payload| payload.deinit(allocator),
            .proof_card => |*payload| payload.deinit(allocator),
            .command_outcome => |*payload| payload.deinit(allocator),
            .verified_patch => |*payload| payload.deinit(allocator),
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
        .verified_patch => |patch| {
            try writer.writeAll("\"kind\":\"verified_patch\",\"file\":");
            try json_writer.writeString(writer, patch.file);
            try writer.writeAll(",\"policy_hash\":");
            try json_writer.writeString(writer, patch.policy_hash);
            try writer.writeAll(",\"stats\":{\"total\":");
            try writer.print("{d}", .{patch.stats.total});
            try writer.writeAll(",\"new\":");
            try writer.print("{d}", .{patch.stats.new});
            if (patch.stats.preexisting) |preexisting| {
                try writer.writeAll(",\"preexisting\":");
                try writer.print("{d}", .{preexisting});
            }
            try writer.writeAll("},\"before\":");
            if (patch.before) |b| {
                try json_writer.writeString(writer, b);
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(",\"after\":");
            try json_writer.writeString(writer, patch.after);
            try writer.writeAll(",\"after_properties\":");
            if (patch.after_properties) |p| {
                try writer.writeByte('{');
                inline for (@typeInfo(PropertiesSnapshot).@"struct".fields, 0..) |field, i| {
                    if (i > 0) try writer.writeByte(',');
                    try writer.writeByte('"');
                    try writer.writeAll(field.name);
                    try writer.writeAll("\":");
                    try writer.writeAll(if (@field(p, field.name)) "true" else "false");
                }
                try writer.writeByte('}');
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(",\"post_apply_ok\":");
            try writer.writeAll(if (patch.post_apply_ok) "true" else "false");
            if (patch.post_apply_summary) |s| {
                try writer.writeAll(",\"post_apply_summary\":");
                try json_writer.writeString(writer, s);
            }
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
    if (std.mem.eql(u8, kind_val.string, "verified_patch")) {
        const file = getString(obj, "file") orelse return error.InvalidUiPayload;
        const policy_hash = getString(obj, "policy_hash") orelse return error.InvalidUiPayload;
        const stats_val = obj.get("stats") orelse return error.InvalidUiPayload;
        if (stats_val != .object) return error.InvalidUiPayload;
        const stats_obj = stats_val.object;
        const after = getString(obj, "after") orelse return error.InvalidUiPayload;
        const before_opt = try getOptionalString(obj, "before");
        const post_apply_ok = getBool(obj, "post_apply_ok") orelse return error.InvalidUiPayload;

        const props_val = obj.get("after_properties") orelse return error.InvalidUiPayload;
        const properties: ?PropertiesSnapshot = switch (props_val) {
            .null => null,
            .object => |po| blk: {
                var snapshot: PropertiesSnapshot = undefined;
                inline for (@typeInfo(PropertiesSnapshot).@"struct".fields) |field| {
                    @field(snapshot, field.name) = getBool(po, field.name) orelse return error.InvalidUiPayload;
                }
                break :blk snapshot;
            },
            else => return error.InvalidUiPayload,
        };

        const file_copy = try allocator.dupe(u8, file);
        errdefer allocator.free(file_copy);
        const policy_copy = try allocator.dupe(u8, policy_hash);
        errdefer allocator.free(policy_copy);
        const before_copy: ?[]u8 = if (before_opt) |b| try allocator.dupe(u8, b) else null;
        errdefer if (before_copy) |b| allocator.free(b);
        const after_copy = try allocator.dupe(u8, after);
        errdefer allocator.free(after_copy);
        const post_apply_summary_copy: ?[]u8 = blk: {
            const s = try getOptionalString(obj, "post_apply_summary");
            break :blk if (s) |text| try allocator.dupe(u8, text) else null;
        };
        errdefer if (post_apply_summary_copy) |s| allocator.free(s);

        return .{ .verified_patch = .{
            .file = file_copy,
            .policy_hash = policy_copy,
            .stats = .{
                .total = @intCast(getUnsigned(stats_obj, "total") orelse return error.InvalidUiPayload),
                .new = @intCast(getUnsigned(stats_obj, "new") orelse return error.InvalidUiPayload),
                .preexisting = if (getUnsigned(stats_obj, "preexisting")) |preexisting|
                    @intCast(preexisting)
                else
                    null,
            },
            .before = before_copy,
            .after = after_copy,
            .after_properties = properties,
            .post_apply_ok = post_apply_ok,
            .post_apply_summary = post_apply_summary_copy,
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

test "verified_patch payload round-trips with properties and optional before" {
    var payload: UiPayload = .{ .verified_patch = .{
        .file = try testing.allocator.dupe(u8, "handler.ts"),
        .policy_hash = try testing.allocator.dupe(u8, "a" ** 64),
        .stats = .{ .total = 1, .new = 0, .preexisting = 1 },
        .before = try testing.allocator.dupe(u8, "old content"),
        .after = try testing.allocator.dupe(u8, "new content"),
        .after_properties = .{
            .pure = true,
            .read_only = false,
            .deterministic = true,
            .retry_safe = true,
            .idempotent = true,
            .state_isolated = true,
            .injection_safe = true,
            .fault_covered = false,
        },
        .post_apply_ok = true,
        .post_apply_summary = null,
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .verified_patch => |patch| {
            try testing.expectEqualStrings("handler.ts", patch.file);
            try testing.expectEqualStrings("a" ** 64, patch.policy_hash);
            try testing.expectEqual(@as(u32, 1), patch.stats.total);
            try testing.expectEqual(@as(u32, 0), patch.stats.new);
            try testing.expect(patch.before != null);
            try testing.expectEqualStrings("old content", patch.before.?);
            try testing.expectEqualStrings("new content", patch.after);
            try testing.expect(patch.after_properties != null);
            try testing.expect(patch.after_properties.?.pure);
            try testing.expect(!patch.after_properties.?.read_only);
            try testing.expect(patch.after_properties.?.retry_safe);
            try testing.expect(!patch.after_properties.?.fault_covered);
            try testing.expect(patch.post_apply_ok);
            try testing.expect(patch.post_apply_summary == null);
        },
        else => return error.TestFailed,
    }
}

test "verified_patch payload round-trips with null before and post-apply note" {
    var payload: UiPayload = .{ .verified_patch = .{
        .file = try testing.allocator.dupe(u8, "new.ts"),
        .policy_hash = try testing.allocator.dupe(u8, "b" ** 64),
        .stats = .{ .total = 0, .new = 0, .preexisting = null },
        .before = null,
        .after = try testing.allocator.dupe(u8, "export default {}"),
        .after_properties = null,
        .post_apply_ok = false,
        .post_apply_summary = try testing.allocator.dupe(u8, "verify_paths regressed"),
    } };
    defer payload.deinit(testing.allocator);

    var roundtripped = try roundTrip(testing.allocator, payload);
    defer roundtripped.deinit(testing.allocator);

    switch (roundtripped) {
        .verified_patch => |patch| {
            try testing.expectEqualStrings("new.ts", patch.file);
            try testing.expect(patch.before == null);
            try testing.expect(patch.after_properties == null);
            try testing.expect(!patch.post_apply_ok);
            try testing.expect(patch.post_apply_summary != null);
            try testing.expectEqualStrings("verify_paths regressed", patch.post_apply_summary.?);
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
