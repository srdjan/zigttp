//! Shared deterministic application for compiler-native repair intents.

const std = @import("std");

pub const Intent = struct {
    plan_id: []const u8,
    intent_kind: []const u8,
    line: u32,
    template: []const u8,
};

pub fn applyIntent(
    allocator: std.mem.Allocator,
    source: []const u8,
    intent: Intent,
) ![]u8 {
    if (std.mem.eql(u8, intent.intent_kind, "insert_guard_before_line")) {
        return insertTemplateBeforeLine(allocator, source, intent.line, intent.template);
    }
    if (std.mem.eql(u8, intent.intent_kind, "add_trailing_return")) {
        return insertTemplateBeforeLastClosingBrace(allocator, source, intent.template);
    }
    return error.UnsupportedRepairIntent;
}

pub fn pathEscapesWorkspace(path: []const u8) bool {
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

const testing = std.testing;

test "insert guard intent preserves target indentation" {
    const source =
        \\function handler(req: Request): Response {
        \\  const data = auth.value;
        \\  return Response.json({ data });
        \\}
    ;
    const out = try applyIntent(testing.allocator, source, .{
        .plan_id = "rp_001",
        .intent_kind = "insert_guard_before_line",
        .line = 2,
        .template = "if (!auth.ok) return Response.json({ error: auth.error }, { status: 400 });",
    });
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "  if (!auth.ok)") != null);
    try testing.expect(std.mem.indexOf(u8, out, "  const data") != null);
}

test "add trailing return intent inserts inside the outer scope" {
    const source =
        \\function handler(req: Request): Response {
        \\  const data = auth.value;
        \\}
    ;
    const out = try applyIntent(testing.allocator, source, .{
        .plan_id = "rp_002",
        .intent_kind = "add_trailing_return",
        .line = 3,
        .template = "return Response.json({ data });",
    });
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "  return Response.json") != null);
    const return_idx = std.mem.indexOf(u8, out, "return Response.json").?;
    const brace_idx = std.mem.lastIndexOfScalar(u8, out, '}').?;
    try testing.expect(return_idx < brace_idx);
}
