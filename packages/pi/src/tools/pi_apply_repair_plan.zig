//! pi_apply_repair_plan - turn one compiler repair intent into a verified
//! in-memory edit candidate. This tool never writes files.

const std = @import("std");
const zigts = @import("zigts");
const edit_simulate = @import("zigts_cli").edit_simulate;
const registry_mod = @import("../registry/registry.zig");
const ui_payload = @import("../ui_payload.zig");
const common = @import("common.zig");

const json_utils = zigts.json_utils;

const name = "pi_apply_repair_plan";

pub const tool: registry_mod.ToolDef = .{
    .name = name,
    .label = "apply-repair-plan",
    .description =
    \\Dry-run a single pi_repair_plan entry into proposed source and
    \\compiler-verify the candidate. This tool never writes files. v1 only
    \\supports deterministic line insertion intents: insert_guard_before_line
    \\and add_trailing_return. Unsupported repair intents return ok:false
    \\with a typed reason so the agent can fall back to manual editing.
    ,
    .input_schema =
    \\{"type":"object","properties":{"path":{"type":"string"},"plan":{"type":"object"},"source":{"type":"string","description":"Optional source snapshot; when omitted, the workspace file is read."}},"required":["path","plan"]}
    ,
    .decode_json = registry_mod.helpers.decodeJsonPassthrough,
    .execute = execute,
};

const RepairIntent = struct {
    plan_id: []const u8,
    intent_kind: []const u8,
    line: u32,
    template: []const u8,
};

fn execute(
    allocator: std.mem.Allocator,
    args: []const []const u8,
) anyerror!registry_mod.ToolResult {
    if (args.len == 0) return registry_mod.ToolResult.err(allocator, name ++ ": requires a JSON input argument\n");

    var parsed = std.json.parseFromSlice(std.json.Value, allocator, args[0], .{}) catch {
        return registry_mod.ToolResult.err(allocator, name ++ ": invalid JSON input\n");
    };
    defer parsed.deinit();
    if (parsed.value != .object) return registry_mod.ToolResult.err(allocator, name ++ ": expected JSON object\n");
    const obj = parsed.value.object;

    const path_value = obj.get("path") orelse return registry_mod.ToolResult.err(allocator, name ++ ": missing \"path\"\n");
    const plan_value = obj.get("plan") orelse return registry_mod.ToolResult.err(allocator, name ++ ": missing \"plan\"\n");
    if (path_value != .string or plan_value != .object) {
        return registry_mod.ToolResult.err(allocator, name ++ ": \"path\" must be a string and \"plan\" must be an object\n");
    }
    const path = path_value.string;
    const intent = parseRepairIntent(plan_value) catch {
        return registry_mod.ToolResult.err(allocator, name ++ ": plan must contain id and edit_intent { kind, line, column, template }\n");
    };

    const source = if (obj.get("source")) |source_value| blk: {
        if (source_value != .string) return registry_mod.ToolResult.err(allocator, name ++ ": \"source\" must be a string when present\n");
        if (pathEscapesWorkspace(path)) return registry_mod.ToolResult.err(allocator, name ++ ": path escapes workspace\n");
        break :blk try allocator.dupe(u8, source_value.string);
    } else blk: {
        const root = try common.workspaceRoot(allocator);
        defer allocator.free(root);
        const absolute = try common.resolveInsideWorkspace(allocator, root, path);
        defer allocator.free(absolute);
        break :blk zigts.file_io.readFile(allocator, absolute, common.default_output_limit) catch |e| {
            return registry_mod.ToolResult.errFmt(
                allocator,
                name ++ ": failed to read {s}: {s}\n",
                .{ absolute, @errorName(e) },
            );
        };
    };
    defer allocator.free(source);

    const proposed = applyIntent(allocator, source, intent) catch |e| switch (e) {
        error.UnsupportedRepairIntent => {
            return try unsupportedResult(allocator, intent);
        },
        error.InvalidRepairLine => {
            return try invalidLineResult(allocator, intent);
        },
        else => return e,
    };
    defer allocator.free(proposed);

    var result = try edit_simulate.simulate(allocator, .{
        .file = path,
        .content = proposed,
        .before = source,
    });
    defer result.deinit(allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;

    try w.writeAll("{\"ok\":");
    try w.writeAll(if (result.new_count == 0) "true" else "false");
    try w.writeAll(",\"applied\":false,\"path\":");
    try json_utils.writeJsonString(w, path);
    try w.writeAll(",\"plan_id\":");
    try json_utils.writeJsonString(w, intent.plan_id);
    try w.writeAll(",\"intent_kind\":");
    try json_utils.writeJsonString(w, intent.intent_kind);
    try w.writeAll(",\"proposed_content\":");
    try json_utils.writeJsonString(w, proposed);
    try w.writeAll(",\"verification\":");
    try edit_simulate.writeResultJson(w, &result);
    try w.writeByte('}');
    try w.writeByte('\n');

    buf = aw.toArrayList();
    const llm_text = try buf.toOwnedSlice(allocator);
    errdefer allocator.free(llm_text);
    const verification_summary = try std.fmt.allocPrint(
        allocator,
        "{d} total, {d} new, {d} preexisting",
        .{ result.total, result.new_count, result.preexisting_count },
    );
    defer allocator.free(verification_summary);
    var payload: ui_payload.UiPayload = .{ .repair_candidate = try ui_payload.RepairCandidatePayload.init(
        allocator,
        path,
        intent.plan_id,
        intent.intent_kind,
        proposed,
        result.new_count == 0,
        verification_summary,
        .{
            .total = result.total,
            .new = result.new_count,
            .preexisting = result.preexisting_count,
        },
    ) };
    errdefer payload.deinit(allocator);
    return .{
        .ok = result.new_count == 0,
        .llm_text = llm_text,
        .ui_payload = payload,
    };
}

fn parseRepairIntent(plan_value: std.json.Value) !RepairIntent {
    if (plan_value != .object) return error.InvalidToolArgsJson;
    const plan = plan_value.object;
    const id_value = plan.get("id") orelse return error.InvalidToolArgsJson;
    const edit_intent_value = plan.get("edit_intent") orelse return error.InvalidToolArgsJson;
    if (id_value != .string or edit_intent_value != .object) return error.InvalidToolArgsJson;

    const edit_intent = edit_intent_value.object;
    const kind_value = edit_intent.get("kind") orelse return error.InvalidToolArgsJson;
    const line_value = edit_intent.get("line") orelse return error.InvalidToolArgsJson;
    const column_value = edit_intent.get("column") orelse return error.InvalidToolArgsJson;
    const template_value = edit_intent.get("template") orelse return error.InvalidToolArgsJson;
    if (kind_value != .string or line_value != .integer or column_value != .integer or template_value != .string) {
        return error.InvalidToolArgsJson;
    }
    if (line_value.integer < 1 or line_value.integer > std.math.maxInt(u32)) return error.InvalidToolArgsJson;
    if (column_value.integer < 0 or column_value.integer > std.math.maxInt(u32)) return error.InvalidToolArgsJson;
    return .{
        .plan_id = id_value.string,
        .intent_kind = kind_value.string,
        .line = @intCast(line_value.integer),
        .template = template_value.string,
    };
}

fn applyIntent(
    allocator: std.mem.Allocator,
    source: []const u8,
    intent: RepairIntent,
) ![]u8 {
    if (std.mem.eql(u8, intent.intent_kind, "insert_guard_before_line")) {
        return insertTemplateBeforeLine(allocator, source, intent.line, intent.template);
    }
    if (std.mem.eql(u8, intent.intent_kind, "add_trailing_return")) {
        return insertTemplateBeforeLastClosingBrace(allocator, source, intent.template);
    }
    return error.UnsupportedRepairIntent;
}

fn pathEscapesWorkspace(path: []const u8) bool {
    if (std.fs.path.isAbsolute(path)) return true;
    var parts = std.mem.tokenizeScalar(u8, path, std.fs.path.sep);
    while (parts.next()) |part| {
        if (std.mem.eql(u8, part, "..")) return true;
    }
    return false;
}

fn insertTemplateBeforeLine(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
    template: []const u8,
) ![]u8 {
    const offset = lineStartOffset(source, line) orelse return error.InvalidRepairLine;
    const line_end = std.mem.indexOfScalarPos(u8, source, offset, '\n') orelse source.len;
    const indent = leadingWhitespace(source[offset..line_end]);
    return spliceLine(allocator, source, offset, indent, template);
}

fn insertTemplateBeforeLastClosingBrace(
    allocator: std.mem.Allocator,
    source: []const u8,
    template: []const u8,
) ![]u8 {
    var scan = source.len;
    while (scan > 0) {
        scan -= 1;
        if (source[scan] != '}') continue;
        const line_start = lineStartBeforeOffset(source, scan);
        const indent = leadingWhitespace(source[line_start..scan]);
        const inner_indent = try std.fmt.allocPrint(allocator, "{s}  ", .{indent});
        defer allocator.free(inner_indent);
        return spliceLine(allocator, source, line_start, inner_indent, template);
    }
    return error.InvalidRepairLine;
}

fn spliceLine(
    allocator: std.mem.Allocator,
    source: []const u8,
    offset: usize,
    indent: []const u8,
    template: []const u8,
) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.appendSlice(allocator, source[0..offset]);
    try out.appendSlice(allocator, indent);
    try out.appendSlice(allocator, template);
    try out.append(allocator, '\n');
    try out.appendSlice(allocator, source[offset..]);
    return try out.toOwnedSlice(allocator);
}

fn lineStartOffset(source: []const u8, line: u32) ?usize {
    if (line == 0) return null;
    if (line == 1) return 0;
    var current: u32 = 1;
    for (source, 0..) |ch, i| {
        if (ch != '\n') continue;
        current += 1;
        if (current == line) return i + 1;
    }
    return null;
}

fn lineStartBeforeOffset(source: []const u8, offset: usize) usize {
    var i = offset;
    while (i > 0) {
        if (source[i - 1] == '\n') return i;
        i -= 1;
    }
    return 0;
}

fn leadingWhitespace(line: []const u8) []const u8 {
    var i: usize = 0;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {}
    return line[0..i];
}

fn unsupportedResult(
    allocator: std.mem.Allocator,
    intent: RepairIntent,
) !registry_mod.ToolResult {
    return jsonFailure(
        allocator,
        intent,
        "unsupported_repair_intent",
        "pi_apply_repair_plan v1 only supports insert_guard_before_line and add_trailing_return",
    );
}

fn invalidLineResult(
    allocator: std.mem.Allocator,
    intent: RepairIntent,
) !registry_mod.ToolResult {
    return jsonFailure(
        allocator,
        intent,
        "invalid_repair_line",
        "repair intent line does not exist in the source snapshot",
    );
}

fn jsonFailure(
    allocator: std.mem.Allocator,
    intent: RepairIntent,
    reason: []const u8,
    message: []const u8,
) !registry_mod.ToolResult {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    const w = &aw.writer;
    try w.writeAll("{\"ok\":false,\"applied\":false,\"plan_id\":");
    try json_utils.writeJsonString(w, intent.plan_id);
    try w.writeAll(",\"intent_kind\":");
    try json_utils.writeJsonString(w, intent.intent_kind);
    try w.writeAll(",\"reason\":");
    try json_utils.writeJsonString(w, reason);
    try w.writeAll(",\"message\":");
    try json_utils.writeJsonString(w, message);
    try w.writeAll("}\n");

    buf = aw.toArrayList();
    return .{ .ok = false, .llm_text = try buf.toOwnedSlice(allocator) };
}

const testing = std.testing;

test "insertTemplateBeforeLine preserves target indentation" {
    const source =
        \\function handler(req: Request): Response {
        \\  const data = auth.value;
        \\  return Response.json({ data });
        \\}
    ;
    const out = try insertTemplateBeforeLine(
        testing.allocator,
        source,
        2,
        "if (!auth.ok) return Response.json({ error: auth.error }, { status: 400 });",
    );
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "  if (!auth.ok)") != null);
    try testing.expect(std.mem.indexOf(u8, out, "  const data") != null);
}

test "insertTemplateBeforeLastClosingBrace inserts inside the outer scope" {
    const source =
        \\function handler(req: Request): Response {
        \\  const data = auth.value;
        \\}
    ;
    const out = try insertTemplateBeforeLastClosingBrace(
        testing.allocator,
        source,
        "return Response.json({ data });",
    );
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "  return Response.json") != null);
    const return_idx = std.mem.indexOf(u8, out, "return Response.json").?;
    const brace_idx = std.mem.lastIndexOfScalar(u8, out, '}').?;
    try testing.expect(return_idx < brace_idx);
}

test "unsupported intent returns typed failure" {
    const intent: RepairIntent = .{
        .plan_id = "rp_001",
        .intent_kind = "replace_sink_expression",
        .line = 2,
        .template = "replace it",
    };
    var result = try unsupportedResult(testing.allocator, intent);
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "unsupported_repair_intent") != null);
}

test "execute dry-runs an add_trailing_return intent" {
    const input =
        \\{"path":"handler.ts","source":"function handler(req: Request): Response {\n  const data = auth.value;\n}","plan":{"id":"rp_002","edit_intent":{"kind":"add_trailing_return","line":3,"column":1,"template":"return Response.json({ data: auth.value });"}}}
    ;
    var result = try execute(testing.allocator, &.{input});
    defer result.deinit(testing.allocator);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"applied\":false") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "return Response.json") != null);
    try testing.expect(result.ui_payload != null);
    switch (result.ui_payload.?) {
        .repair_candidate => |candidate| {
            try testing.expectEqualStrings("add_trailing_return", candidate.intent_kind);
            try testing.expect(std.mem.indexOf(u8, candidate.proposed_content, "return Response.json") != null);
            const return_idx = std.mem.indexOf(u8, candidate.proposed_content, "return Response.json").?;
            const brace_idx = std.mem.lastIndexOfScalar(u8, candidate.proposed_content, '}').?;
            try testing.expect(return_idx < brace_idx);
        },
        else => return error.TestFailed,
    }
}

test "execute returns typed failure for unsupported intent" {
    const input =
        \\{"path":"handler.ts","source":"function handler() {}","plan":{"id":"rp_003","edit_intent":{"kind":"replace_sink_expression","line":1,"column":1,"template":"replace"}}}
    ;
    var result = try execute(testing.allocator, &.{input});
    defer result.deinit(testing.allocator);
    try testing.expect(!result.ok);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "unsupported_repair_intent") != null);
    try testing.expect(result.ui_payload == null);
}

test "execute dry-runs a source-backed guard insertion" {
    const input =
        \\{"path":"handler.ts","source":"function handler(req: Request): Response {\n  const data = auth.value;\n  return Response.json({ data });\n}","plan":{"id":"rp_001","edit_intent":{"kind":"insert_guard_before_line","line":2,"column":14,"template":"if (!auth.ok) return Response.json({ error: auth.error }, { status: 400 });"}}}
    ;
    var result = try execute(testing.allocator, &.{input});
    defer result.deinit(testing.allocator);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "\"applied\":false") != null);
    try testing.expect(std.mem.indexOf(u8, result.llm_text, "if (!auth.ok)") != null);
    try testing.expect(result.ui_payload != null);
    switch (result.ui_payload.?) {
        .repair_candidate => |candidate| {
            try testing.expectEqualStrings("handler.ts", candidate.path);
            try testing.expectEqualStrings("rp_001", candidate.plan_id);
            try testing.expectEqualStrings("insert_guard_before_line", candidate.intent_kind);
            try testing.expect(std.mem.indexOf(u8, candidate.proposed_content, "if (!auth.ok)") != null);
        },
        else => return error.TestFailed,
    }
}
