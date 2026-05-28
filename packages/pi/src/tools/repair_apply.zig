//! Shared deterministic application for compiler-native repair intents.
//!
//! Slice G of the expert-strategy §5 roadmap extended the dispatch to cover
//! four line-local canonicalize refactors (`replace_let_with_const`,
//! `canonicalize_for_of_const`, `replace_arrow_with_function`,
//! `replace_export_arrow_with_function`). The fifth canonicalize refactor —
//! `canonicalize_capability_key_alias` — needs cross-line scope analysis
//! that lives in `tools/canonicalize.zig`, so the AST rewrite tool runs that
//! one through `canonicalize.collect` directly rather than through this
//! source-only `applyIntent`.

const std = @import("std");
const canonicalize = @import("zigts_cli").canonicalize;

pub const Intent = struct {
    plan_id: []const u8,
    intent_kind: []const u8,
    line: u32,
    template: []const u8,
};

/// Recognised intent kinds. Kept in sync with `RepairIntent` in
/// `packages/zigts/src/repair_intent.zig`: the compiler emits the typed
/// enum, this enum is what the apply pipeline actually executes.
/// `canonicalize_capability_key_alias` is intentionally absent — that
/// refactor requires file-level scope analysis the AST tool runs through
/// `canonicalize.collect` rather than through this source-only dispatch.
pub const RepairKind = enum {
    insert_guard_before_line,
    add_trailing_return,
    replace_let_with_const,
    canonicalize_for_of_const,
    replace_arrow_with_function,
    replace_export_arrow_with_function,

    pub fn fromString(s: []const u8) ?RepairKind {
        return std.meta.stringToEnum(RepairKind, s);
    }
};

pub fn applyIntent(
    allocator: std.mem.Allocator,
    source: []const u8,
    intent: Intent,
) ![]u8 {
    const kind = RepairKind.fromString(intent.intent_kind) orelse
        return error.UnsupportedRepairIntent;
    return switch (kind) {
        .insert_guard_before_line => insertTemplateBeforeLine(allocator, source, intent.line, intent.template),
        .add_trailing_return => insertTemplateBeforeLastClosingBrace(allocator, source, intent.template),
        .replace_let_with_const, .canonicalize_for_of_const => applyAvoidableLet(allocator, source, intent.line),
        .replace_arrow_with_function, .replace_export_arrow_with_function => applyCanonicalFunction(allocator, source, intent.line),
    };
}

fn applyAvoidableLet(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
) ![]u8 {
    const target = canonicalize.sourceLine(source, line) orelse return error.InvalidRepairLine;
    const replacement = canonicalize.avoidableLetReplacement(allocator, target) catch |e| switch (e) {
        error.UnsupportedRefactor => return error.UnsupportedRepairIntent,
        else => return e,
    };
    defer allocator.free(replacement);
    return replaceLine(allocator, source, line, replacement);
}

fn applyCanonicalFunction(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
) ![]u8 {
    const target = canonicalize.sourceLine(source, line) orelse return error.InvalidRepairLine;
    const indent = leadingWhitespace(target);
    const inner = canonicalize.canonicalFunctionReplacement(allocator, target) catch |e| switch (e) {
        error.UnsupportedRefactor => return error.UnsupportedRepairIntent,
        else => return e,
    };
    defer allocator.free(inner);
    const replacement = try std.fmt.allocPrint(allocator, "{s}{s}", .{ indent, inner });
    defer allocator.free(replacement);
    return replaceLine(allocator, source, line, replacement);
}

fn replaceLine(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
    replacement: []const u8,
) ![]u8 {
    const offset = lineStartOffset(source, line) orelse return error.InvalidRepairLine;
    const line_end = std.mem.indexOfScalarPos(u8, source, offset, '\n') orelse source.len;
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.appendSlice(allocator, source[0..offset]);
    try out.appendSlice(allocator, replacement);
    try out.appendSlice(allocator, source[line_end..]);
    return try out.toOwnedSlice(allocator);
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

test "replace_let_with_const intent rewrites a local let line" {
    const source =
        \\function handler(req: Request): Response {
        \\  let count = 1;
        \\  return Response.json({ count });
        \\}
    ;
    const out = try applyIntent(testing.allocator, source, .{
        .plan_id = "rp_g1",
        .intent_kind = "replace_let_with_const",
        .line = 2,
        .template = "",
    });
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "  const count = 1;") != null);
    try testing.expect(std.mem.indexOf(u8, out, "let ") == null);
}

test "canonicalize_for_of_const intent rewrites a for-of let binding" {
    const source =
        \\function handler(req: Request): Response {
        \\  const items = [1, 2];
        \\  for (let item of items) {
        \\    Response.json({ item });
        \\  }
        \\  return Response.json({ ok: true });
        \\}
    ;
    const out = try applyIntent(testing.allocator, source, .{
        .plan_id = "rp_g2",
        .intent_kind = "canonicalize_for_of_const",
        .line = 3,
        .template = "",
    });
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "  for (const item of items)") != null);
}

test "replace_arrow_with_function intent rewrites an arrow helper" {
    const source =
        \\const parse = (x: number): number => x;
        \\function handler(req: Request): Response {
        \\  return Response.json({ a: parse(1) });
        \\}
    ;
    const out = try applyIntent(testing.allocator, source, .{
        .plan_id = "rp_g3",
        .intent_kind = "replace_arrow_with_function",
        .line = 1,
        .template = "",
    });
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "function parse(x: number): number { return x; }") != null);
}

test "replace_export_arrow_with_function rewrites an exported arrow helper" {
    const source =
        \\export const load = (id: string): Response => Response.text(id);
        \\function handler(req: Request): Response {
        \\  return load("x");
        \\}
    ;
    const out = try applyIntent(testing.allocator, source, .{
        .plan_id = "rp_g4",
        .intent_kind = "replace_export_arrow_with_function",
        .line = 1,
        .template = "",
    });
    defer testing.allocator.free(out);
    try testing.expect(std.mem.indexOf(u8, out, "export function load(id: string): Response { return Response.text(id); }") != null);
}

test "canonicalize_capability_key_alias is not dispatched source-only" {
    // capability alias needs cross-line scope analysis through
    // canonicalize.collect; the AST tool runs that path itself rather
    // than going through applyIntent.
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req: Request): Response {
        \\  let key = "API_KEY";
        \\  const value = env(key);
        \\  return Response.json({ value });
        \\}
    ;
    try testing.expectError(error.UnsupportedRepairIntent, applyIntent(testing.allocator, source, .{
        .plan_id = "rp_g5",
        .intent_kind = "canonicalize_capability_key_alias",
        .line = 3,
        .template = "",
    }));
}
