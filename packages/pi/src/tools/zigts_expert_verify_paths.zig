//! `ToolResult.ok` mirrors the v1 envelope's `ok` field so callers can branch
//! on the tool result without parsing the body.

const std = @import("std");
const verify_paths_core = @import("zigts_cli").verify_paths_core;
const registry_mod = @import("../registry/registry.zig");
const ui_payload = @import("../ui_payload.zig");

const name = "zigts_expert_verify_paths";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "verify handler(s)",
    .description = "Run full analysis on one or more handler files and emit the v1 envelope.",
    .input_schema = "{\"type\":\"object\",\"properties\":{\"paths\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"One or more handler paths.\"}},\"required\":[\"paths\"]}",
    .decode_json = decodeJson,
    .execute = execute,
};

fn decodeJson(
    allocator: std.mem.Allocator,
    args_json: []const u8,
) ![]const []const u8 {
    return registry_mod.helpers.decodeStringArrayField(allocator, args_json, "paths");
}

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) {
        return registry_mod.ToolResult.err(allocator, "zigts_expert_verify_paths requires at least one path\n");
    }

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);

    const outcome = try verify_paths_core.writeJsonEnvelope(allocator, &aw.writer, args);

    buf = aw.toArrayList();
    const llm_text = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(llm_text);

    return .{
        .ok = outcome.ok,
        .llm_text = llm_text,
        .ui_payload = buildDiagnosticsPayload(allocator, llm_text) catch null,
    };
}

fn buildDiagnosticsPayload(
    allocator: std.mem.Allocator,
    llm_text: []const u8,
) !ui_payload.UiPayload {
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, llm_text, .{});
    defer parsed.deinit();
    if (parsed.value != .object) return error.InvalidDiagnosticsEnvelope;
    const obj = parsed.value.object;
    const checked_files = obj.get("checked_files") orelse return error.InvalidDiagnosticsEnvelope;
    const violations = obj.get("violations") orelse return error.InvalidDiagnosticsEnvelope;
    if (checked_files != .array or violations != .array) return error.InvalidDiagnosticsEnvelope;

    const items = try allocator.alloc(ui_payload.DiagnosticItem, violations.array.items.len);
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
    while (i < violations.array.items.len) : (i += 1) {
        const diag = violations.array.items[i];
        if (diag != .object) return error.InvalidDiagnosticsEnvelope;
        const diag_obj = diag.object;
        const code = diag_obj.get("code") orelse return error.InvalidDiagnosticsEnvelope;
        const severity = diag_obj.get("severity") orelse return error.InvalidDiagnosticsEnvelope;
        const message = diag_obj.get("message") orelse return error.InvalidDiagnosticsEnvelope;
        const file = diag_obj.get("file") orelse return error.InvalidDiagnosticsEnvelope;
        const line = diag_obj.get("line") orelse return error.InvalidDiagnosticsEnvelope;
        const column = diag_obj.get("column") orelse return error.InvalidDiagnosticsEnvelope;
        if (code != .string or severity != .string or message != .string or file != .string or line != .integer or column != .integer) {
            return error.InvalidDiagnosticsEnvelope;
        }
        items[i] = try ui_payload.DiagnosticItem.init(
            allocator,
            code.string,
            severity.string,
            file.string,
            @intCast(line.integer),
            @intCast(column.integer),
            message.string,
            null,
        );
    }

    return .{ .diagnostics = .{
        .summary = try std.fmt.allocPrint(
            allocator,
            "{d} checked file(s), {d} diagnostic(s)",
            .{ checked_files.array.items.len, items.len },
        ),
        .items = items,
    } };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "missing args returns not-ok body" {
    var result = try execute(testing.allocator, &.{});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "requires at least one path") != null);
}

test "execute on a file that does not exist emits ZTS000 envelope" {
    var result = try execute(testing.allocator, &.{"/tmp/zigts-pi-does-not-exist.ts"});
    defer result.deinit(testing.allocator);

    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"ok\":false") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"ZTS000\"") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"policy_version\":\"2026.04.2\"") != null);
    try testing.expect(result.ui_payload != null);
}
