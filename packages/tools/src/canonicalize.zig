//! Compiler-authored canonical refactor previews.
//!
//! Emits local rewrite intents only. Callers still apply any replacement
//! through edit-simulate / compiler veto; this module never writes source.

const std = @import("std");
const zigts = @import("zigts");
const precompile = @import("precompile.zig");
const edit_simulate = @import("edit_simulate.zig");
const writeJsonString = zigts.handler_contract.writeJsonString;

pub const Refactor = struct {
    kind: []const u8,
    line: u32,
    column: u16,
    message: []const u8,
    replacement: []const u8,
    original_line: ?[]const u8 = null,
};

pub const Result = struct {
    file: []const u8,
    refactors: std.ArrayListUnmanaged(Refactor) = .empty,

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        for (self.refactors.items) |*r| freeRefactorOwned(allocator, r);
        self.refactors.deinit(allocator);
        self.* = .{ .file = "" };
    }
};

pub const SimulationSummary = struct {
    ok: bool,
    total: u32,
    new_count: u32,
    preexisting_count: u32,
};

pub fn collect(allocator: std.mem.Allocator, file: []const u8) !Result {
    const source = try zigts.file_io.readFile(allocator, file, 10 * 1024 * 1024);
    defer allocator.free(source);

    var check = try precompile.runCheckOnly(allocator, file, null, true, null);
    defer check.deinit(allocator);

    var result = Result{ .file = file };
    errdefer result.deinit(allocator);

    const diagnostics = check.json_diagnostics.items;
    for (diagnostics) |diag| {
        const line = sourceLine(source, diag.line) orelse continue;
        if (std.mem.eql(u8, diag.code, "ZTS608") or std.mem.eql(u8, diag.code, "ZTS609")) {
            const replacement = canonicalFunctionReplacement(allocator, line) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, &result, .{
                .kind = if (std.mem.eql(u8, diag.code, "ZTS608"))
                    "canonicalize_arrow_helper"
                else
                    "canonicalize_export_function",
                .line = diag.line,
                .column = diag.column,
                .message = diag.message,
                .replacement = replacement,
                .original_line = try allocator.dupe(u8, line),
            });
        } else if (std.mem.eql(u8, diag.code, "ZTS604")) {
            const replacement = avoidableLetReplacement(allocator, line) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, &result, .{
                .kind = if (std.mem.indexOf(u8, line, "for (let ") != null)
                    "canonicalize_for_of_const"
                else
                    "canonicalize_let_const",
                .line = diag.line,
                .column = diag.column,
                .message = diag.message,
                .replacement = replacement,
                .original_line = try allocator.dupe(u8, line),
            });
        } else if (std.mem.eql(u8, diag.code, "ZTS613")) {
            const replacement = compoundAssignReplacement(allocator, line) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, &result, .{
                .kind = "canonicalize_compound_assign",
                .line = diag.line,
                .column = diag.column,
                .message = diag.message,
                .replacement = replacement,
                .original_line = try allocator.dupe(u8, line),
            });
        } else if (std.mem.eql(u8, diag.code, "ZTS602")) {
            const replacement = capabilityAliasReplacement(allocator, source, diag, diagnostics) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, &result, replacement);
        }
    }

    return result;
}

fn appendRefactorUnique(allocator: std.mem.Allocator, result: *Result, refactor: Refactor) !void {
    var owned = refactor;
    for (result.refactors.items) |*existing| {
        if (existing.line == owned.line and std.mem.eql(u8, existing.replacement, owned.replacement)) {
            if (std.mem.eql(u8, owned.kind, "canonicalize_capability_key_alias")) {
                existing.kind = owned.kind;
                existing.column = owned.column;
                existing.message = owned.message;
            }
            freeRefactorOwned(allocator, &owned);
            return;
        }
    }
    result.refactors.append(allocator, owned) catch |err| {
        freeRefactorOwned(allocator, &owned);
        return err;
    };
}

fn freeRefactorOwned(allocator: std.mem.Allocator, refactor: *Refactor) void {
    allocator.free(refactor.replacement);
    if (refactor.original_line) |line| allocator.free(line);
    refactor.* = .{
        .kind = "",
        .line = 0,
        .column = 0,
        .message = "",
        .replacement = "",
        .original_line = null,
    };
}

pub fn canonicalFunctionReplacement(allocator: std.mem.Allocator, line: []const u8) ![]u8 {
    const trimmed = std.mem.trim(u8, line, " \t\r\n");
    const is_export = std.mem.startsWith(u8, trimmed, "export const ");
    const prefix_len: usize = if (is_export) "export const ".len else "const ".len;
    if (!is_export and !std.mem.startsWith(u8, trimmed, "const ")) return error.UnsupportedRefactor;

    const rest = trimmed[prefix_len..];
    const eq = assignmentEquals(rest) orelse return error.UnsupportedRefactor;
    const raw_name = std.mem.trim(u8, rest[0..eq], " \t");
    if (raw_name.len == 0) return error.UnsupportedRefactor;
    for (raw_name) |c| {
        if (c == ':' or std.ascii.isWhitespace(c)) return error.UnsupportedRefactor;
    }

    const arrow_source = std.mem.trim(u8, rest[eq + 1 ..], " \t");
    const arrow = std.mem.indexOf(u8, arrow_source, "=>") orelse return error.UnsupportedRefactor;
    const signature = std.mem.trim(u8, arrow_source[0..arrow], " \t");
    if (std.mem.startsWith(u8, signature, "<")) return error.UnsupportedRefactor;
    const needs_param_parens = !std.mem.startsWith(u8, signature, "(");
    const function_signature = if (needs_param_parens)
        try std.fmt.allocPrint(allocator, "({s})", .{signature})
    else
        signature;
    defer if (needs_param_parens) allocator.free(function_signature);
    var body = std.mem.trim(u8, arrow_source[arrow + 2 ..], " \t");
    if (std.mem.endsWith(u8, body, ";")) body = std.mem.trim(u8, body[0 .. body.len - 1], " \t");

    const export_prefix: []const u8 = if (is_export) "export " else "";
    if (std.mem.startsWith(u8, body, "{")) {
        return std.fmt.allocPrint(allocator, "{s}function {s}{s} {s}", .{
            export_prefix,
            raw_name,
            function_signature,
            body,
        });
    }

    return std.fmt.allocPrint(allocator, "{s}function {s}{s} {{ return {s}; }}", .{
        export_prefix,
        raw_name,
        function_signature,
        body,
    });
}

pub fn avoidableLetReplacement(allocator: std.mem.Allocator, line: []const u8) ![]u8 {
    const trimmed_left = trimLeft(line, " \t");
    const indent_len = line.len - trimmed_left.len;
    const indent = line[0..indent_len];
    if (std.mem.startsWith(u8, trimmed_left, "let ")) {
        return std.fmt.allocPrint(allocator, "{s}const {s}", .{
            indent,
            trimmed_left["let ".len..],
        });
    }

    const needle = "for (let ";
    const at = std.mem.indexOf(u8, line, needle) orelse return error.UnsupportedRefactor;
    return std.fmt.allocPrint(allocator, "{s}for (const {s}", .{
        line[0..at],
        line[at + needle.len ..],
    });
}

/// ZTS613: rewrite a compound assignment `lhs OP= rhs` into the explicit
/// `lhs = lhs OP rhs` so the read and the write are both visible.
///
/// Conservative by design: only the arithmetic operators (`+= -= *= /= %=`)
/// are handled, and the target must be a *simple lvalue* — an identifier or a
/// dotted-identifier chain (`a`, `a.b.c`). A target containing a call or index
/// (`f().x`, `a[i]`) is refused with `UnsupportedRefactor`, because expanding
/// it would re-evaluate a possibly side-effecting base expression twice and
/// change behavior. Bitwise/shift compound operators are likewise refused to
/// avoid the `<=`/`<<=` and `>=`/`>>=` lexing ambiguity; the agent falls back
/// to a manual edit there.
pub fn compoundAssignReplacement(allocator: std.mem.Allocator, line: []const u8) ![]u8 {
    const trimmed_left = trimLeft(line, " \t");
    const indent = line[0 .. line.len - trimmed_left.len];

    // Find the compound-assignment `=`: the first `=` immediately preceded by
    // an arithmetic operator and not itself part of `==`.
    var i: usize = indent.len;
    while (i < line.len) : (i += 1) {
        if (line[i] != '=') continue;
        if (i == indent.len) continue;
        if (i + 1 < line.len and line[i + 1] == '=') continue; // ==, ===
        const op_char = line[i - 1];
        if (!isArithmeticOpChar(op_char)) continue;

        const lhs = std.mem.trim(u8, line[indent.len .. i - 1], " \t");
        const rhs = trimLeft(line[i + 1 ..], " \t");
        if (!isSimpleLvalue(lhs)) return error.UnsupportedRefactor;
        if (rhs.len == 0) return error.UnsupportedRefactor;

        return std.fmt.allocPrint(allocator, "{s}{s} = {s} {c} {s}", .{
            indent,
            lhs,
            lhs,
            op_char,
            rhs,
        });
    }
    return error.UnsupportedRefactor;
}

fn isArithmeticOpChar(c: u8) bool {
    return c == '+' or c == '-' or c == '*' or c == '/' or c == '%';
}

/// A simple lvalue is an identifier or dotted-identifier chain: re-evaluating
/// it is side-effect-free, so `lhs = lhs OP rhs` preserves behavior.
fn isSimpleLvalue(s: []const u8) bool {
    if (s.len == 0) return false;
    if (!isIdentStart(s[0])) return false;
    var prev_dot = false;
    for (s, 0..) |c, idx| {
        if (c == '.') {
            // No leading/trailing/double dots.
            if (idx == 0 or idx == s.len - 1 or prev_dot) return false;
            prev_dot = true;
            continue;
        }
        if (!isIdentContinue(c)) return false;
        prev_dot = false;
    }
    return true;
}

const AliasReplacement = struct {
    line: u32,
    replacement: []const u8,
    original_line: []const u8,
};

fn capabilityAliasReplacement(
    allocator: std.mem.Allocator,
    source: []const u8,
    diag: precompile.json_diag.JsonDiagnostic,
    diagnostics: []const precompile.json_diag.JsonDiagnostic,
) !Refactor {
    const call_line = sourceLine(source, diag.line) orelse return error.UnsupportedRefactor;
    const ident = identifierAtColumn(call_line, diag.column) orelse return error.UnsupportedRefactor;
    const alias = findLiteralLetAlias(allocator, source, ident, diag.line, diagnostics) catch |err| switch (err) {
        error.UnsupportedRefactor => return error.UnsupportedRefactor,
        else => return err,
    };
    return .{
        .kind = "canonicalize_capability_key_alias",
        .line = alias.line,
        .column = 1,
        .message = "make capability key alias compiler-visible",
        .replacement = alias.replacement,
        .original_line = alias.original_line,
    };
}

fn identifierAtColumn(line: []const u8, column: u16) ?[]const u8 {
    if (column == 0) return null;
    var start: usize = @as(usize, column) - 1;
    if (start >= line.len) return null;
    while (start < line.len and std.ascii.isWhitespace(line[start])) start += 1;
    if (start >= line.len or !isIdentStart(line[start])) return null;
    var end = start + 1;
    while (end < line.len and isIdentContinue(line[end])) end += 1;
    return line[start..end];
}

fn findLiteralLetAlias(
    allocator: std.mem.Allocator,
    source: []const u8,
    ident: []const u8,
    use_line: u32,
    diagnostics: []const precompile.json_diag.JsonDiagnostic,
) !AliasReplacement {
    const block_start = enclosingBlockStartLine(source, use_line) orelse return error.UnsupportedRefactor;
    var line_num: u32 = 1;
    var start: usize = 0;
    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (line_num >= block_start and line_num < use_line) {
                if (try literalLetAliasReplacement(allocator, source[start..i], ident, line_num, diagnostics)) |replacement| return replacement;
            }
            line_num += 1;
            start = i + 1;
        }
    }
    return error.UnsupportedRefactor;
}

fn literalLetAliasReplacement(
    allocator: std.mem.Allocator,
    line: []const u8,
    ident: []const u8,
    line_num: u32,
    diagnostics: []const precompile.json_diag.JsonDiagnostic,
) !?AliasReplacement {
    if (!hasDiagnosticAt(diagnostics, "ZTS604", line_num)) return null;
    const trimmed_left = trimLeft(line, " \t");
    if (!std.mem.startsWith(u8, trimmed_left, "let ")) return null;
    const rest = trimmed_left["let ".len..];
    if (!std.mem.startsWith(u8, rest, ident)) return null;
    const after_name = rest[ident.len..];
    if (after_name.len > 0 and isIdentContinue(after_name[0])) return null;
    const eq = assignmentEquals(after_name) orelse return null;
    const rhs = trimLeft(after_name[eq + 1 ..], " \t");
    if (!isStaticLiteralExpression(rhs)) return null;
    return .{
        .line = line_num,
        .replacement = try avoidableLetReplacement(allocator, line),
        .original_line = try allocator.dupe(u8, line),
    };
}

fn enclosingBlockStartLine(source: []const u8, use_line: u32) ?u32 {
    var line_num: u32 = 1;
    var start: usize = 0;
    var depth: i32 = 0;
    var best: ?u32 = null;
    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (line_num >= use_line) break;
            updateBlockDepth(source[start..i], line_num, &depth, &best);
            line_num += 1;
            start = i + 1;
        }
    }
    if (line_num < use_line) updateBlockDepth(source[start..], line_num, &depth, &best);
    return best;
}

fn updateBlockDepth(line: []const u8, line_num: u32, depth: *i32, best: *?u32) void {
    for (line) |c| {
        if (c == '{') {
            depth.* += 1;
            best.* = line_num + 1;
        } else if (c == '}') {
            depth.* -= 1;
            if (depth.* < 0) depth.* = 0;
        }
    }
}

fn hasDiagnosticAt(diagnostics: []const precompile.json_diag.JsonDiagnostic, code: []const u8, line: u32) bool {
    for (diagnostics) |diag| {
        if (diag.line == line and std.mem.eql(u8, diag.code, code)) return true;
    }
    return false;
}

fn isStaticLiteralExpression(source: []const u8) bool {
    const trimmed = trimRight(source, " \t\r;");
    if (trimmed.len == 0) return false;
    if (trimmed[0] == '"' or trimmed[0] == '\'') return quotedLiteralConsumes(trimmed, trimmed[0]) == trimmed.len;
    if (trimmed[0] == '`') return quotedLiteralConsumes(trimmed, '`') == trimmed.len and std.mem.indexOf(u8, trimmed, "${") == null;
    for (trimmed) |c| {
        if (!std.ascii.isDigit(c)) return false;
    }
    return true;
}

fn quotedLiteralConsumes(source: []const u8, quote: u8) usize {
    if (source.len == 0 or source[0] != quote) return 0;
    var escaped = false;
    var i: usize = 1;
    while (i < source.len) : (i += 1) {
        const c = source[i];
        if (escaped) {
            escaped = false;
            continue;
        }
        if (c == '\\') {
            escaped = true;
            continue;
        }
        if (c == quote) return i + 1;
    }
    return 0;
}

fn trimLeft(source: []const u8, values: []const u8) []const u8 {
    var start: usize = 0;
    while (start < source.len and std.mem.indexOfScalar(u8, values, source[start]) != null) {
        start += 1;
    }
    return source[start..];
}

fn trimRight(source: []const u8, values: []const u8) []const u8 {
    var end = source.len;
    while (end > 0 and std.mem.indexOfScalar(u8, values, source[end - 1]) != null) {
        end -= 1;
    }
    return source[0..end];
}

fn isIdentStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_' or c == '$';
}

fn isIdentContinue(c: u8) bool {
    return isIdentStart(c) or std.ascii.isDigit(c);
}

fn assignmentEquals(source: []const u8) ?usize {
    for (source, 0..) |c, i| {
        if (c != '=') continue;
        if (i + 1 < source.len and source[i + 1] == '>') continue;
        return i;
    }
    return null;
}

pub fn sourceLine(source: []const u8, line_num: u32) ?[]const u8 {
    if (line_num == 0) return null;
    var current: u32 = 1;
    var start: usize = 0;
    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (current == line_num) return source[start..i];
            current += 1;
            start = i + 1;
        }
    }
    if (current == line_num) return source[start..];
    return null;
}

pub fn applyRefactors(
    allocator: std.mem.Allocator,
    source: []const u8,
    refactors: []const Refactor,
) ![]u8 {
    for (refactors, 0..) |a, i| {
        if (std.mem.indexOfScalar(u8, a.replacement, '\n') != null) return error.UnsupportedRefactor;
        for (refactors[i + 1 ..]) |b| {
            if (a.line == b.line) return error.OverlappingRefactors;
        }
    }

    const applied = try allocator.alloc(bool, refactors.len);
    defer allocator.free(applied);
    @memset(applied, false);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    var line_num: u32 = 1;
    var start: usize = 0;
    for (source, 0..) |c, i| {
        if (c == '\n') {
            try appendAppliedLine(allocator, &out, source[start..i], line_num, refactors, applied);
            try out.append(allocator, '\n');
            line_num += 1;
            start = i + 1;
        }
    }
    if (start < source.len) {
        try appendAppliedLine(allocator, &out, source[start..], line_num, refactors, applied);
    }

    for (applied) |was_applied| {
        if (!was_applied) return error.RefactorLineNotFound;
    }

    return try out.toOwnedSlice(allocator);
}

fn appendAppliedLine(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    line: []const u8,
    line_num: u32,
    refactors: []const Refactor,
    applied: []bool,
) !void {
    for (refactors, 0..) |refactor, i| {
        if (refactor.line != line_num) continue;
        if (refactor.original_line) |original| {
            if (!std.mem.eql(u8, original, line)) return error.StaleRefactorLine;
        }
        try out.appendSlice(allocator, refactor.replacement);
        applied[i] = true;
        return;
    }
    try out.appendSlice(allocator, line);
}

pub fn simulateRefactors(
    allocator: std.mem.Allocator,
    file: []const u8,
    result: *const Result,
) !SimulationSummary {
    const source = try zigts.file_io.readFile(allocator, file, 10 * 1024 * 1024);
    defer allocator.free(source);

    const proposed = try applyRefactors(allocator, source, result.refactors.items);
    defer allocator.free(proposed);

    var simulation = try edit_simulate.simulate(allocator, .{
        .file = file,
        .content = proposed,
        .before = source,
    });
    defer simulation.deinit(allocator);

    return .{
        .ok = simulation.new_count == 0,
        .total = simulation.total,
        .new_count = simulation.new_count,
        .preexisting_count = simulation.preexisting_count,
    };
}

pub fn writeJson(writer: anytype, result: *const Result) !void {
    try writeJsonWithSimulation(writer, result, null);
}

pub fn writeJsonWithSimulation(
    writer: anytype,
    result: *const Result,
    simulation: ?SimulationSummary,
) !void {
    const hash = zigts.rule_registry.policyHash();
    try writer.writeAll("{\"ok\":true,\"file\":");
    try writeJsonString(writer, result.file);
    try writer.writeAll(",\"policy_hash\":");
    try writeJsonString(writer, &hash);
    try writer.writeAll(",\"refactors\":[");
    for (result.refactors.items, 0..) |r, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"kind\":");
        try writeJsonString(writer, r.kind);
        try writer.print(",\"line\":{d},\"column\":{d},\"message\":", .{ r.line, r.column });
        try writeJsonString(writer, r.message);
        try writer.writeAll(",\"replacement\":");
        try writeJsonString(writer, r.replacement);
        try writer.writeByte('}');
    }
    try writer.writeByte(']');
    if (simulation) |sim| {
        try writer.print(
            ",\"simulation\":{{\"ok\":{},\"total\":{d},\"new\":{d},\"preexisting\":{d}}}",
            .{ sim.ok, sim.total, sim.new_count, sim.preexisting_count },
        );
    }
    try writer.writeAll("}\n");
}

pub fn runWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var file: ?[]const u8 = null;
    var json_mode = false;
    var simulate_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (std.mem.eql(u8, arg, "--simulate")) {
            simulate_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-") and file == null) {
            file = arg;
        } else {
            return error.InvalidArgument;
        }
    }
    const path = file orelse return error.MissingArgument;
    if (!json_mode) return error.InvalidArgument;

    var result = try collect(allocator, path);
    defer result.deinit(allocator);
    const simulation = if (simulate_mode)
        try simulateRefactors(allocator, path, &result)
    else
        null;

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
    try writeJsonWithSimulation(&aw.writer, &result, simulation);
    buf = aw.toArrayList();
    if (buf.items.len > 0) _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
}

fn printHelp() void {
    const help =
        \\zigts canonicalize - preview canonical local refactors
        \\
        \\Usage: zigts canonicalize <file> --json [--simulate]
        \\
        \\Emits rewrite intents only. --simulate applies previews in memory and
        \\runs edit-simulate; it never writes source.
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
}

test "canonicalFunctionReplacement rewrites arrow expression" {
    const got = try canonicalFunctionReplacement(std.testing.allocator, "const parse = (x: number): number => x;");
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("function parse(x: number): number { return x; }", got);
}

test "canonicalFunctionReplacement rewrites exported arrow expression" {
    const got = try canonicalFunctionReplacement(std.testing.allocator, "export const load = (id: string): Response => Response.text(id);");
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("export function load(id: string): Response { return Response.text(id); }", got);
}

test "canonicalFunctionReplacement wraps single arrow param" {
    const got = try canonicalFunctionReplacement(std.testing.allocator, "const id = x => x;");
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("function id(x) { return x; }", got);
}

test "canonicalFunctionReplacement rejects typed const bindings" {
    try std.testing.expectError(
        error.UnsupportedRefactor,
        canonicalFunctionReplacement(std.testing.allocator, "const parse: Parser = (x: number): number => x;"),
    );
}

test "canonicalFunctionReplacement rejects generic arrow helpers" {
    try std.testing.expectError(
        error.UnsupportedRefactor,
        canonicalFunctionReplacement(std.testing.allocator, "const id = <T>(x: T): T => x;"),
    );
}

test "avoidableLetReplacement rewrites local let" {
    const got = try avoidableLetReplacement(std.testing.allocator, "    let key = \"API_KEY\";");
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("    const key = \"API_KEY\";", got);
}

test "avoidableLetReplacement rewrites for-of binding" {
    const got = try avoidableLetReplacement(std.testing.allocator, "for (let item of items) {");
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("for (const item of items) {", got);
}

fn writeTempHandler(dir: std.fs.Dir, source: []const u8) ![]const u8 {
    try dir.writeFile(.{ .sub_path = "handler.ts", .data = source });
    const path = try std.testing.allocator.alloc(u8, std.fs.max_path_bytes);
    errdefer std.testing.allocator.free(path);
    const resolved = try dir.realpath("handler.ts", path[0..std.fs.max_path_bytes]);
    return path[0..resolved.len];
}

fn expectCanonicalizeEnvelope(json: []const u8, file: []const u8, expected_count: usize) !std.json.Parsed(std.json.Value) {
    var parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json, .{});
    errdefer parsed.deinit();
    try std.testing.expect(parsed.value == .object);
    const obj = parsed.value.object;
    const ok = obj.get("ok") orelse return error.MissingOk;
    const file_value = obj.get("file") orelse return error.MissingFile;
    const hash = obj.get("policy_hash") orelse return error.MissingPolicyHash;
    const refactors = obj.get("refactors") orelse return error.MissingRefactors;
    try std.testing.expect(ok == .bool and ok.bool);
    try std.testing.expect(file_value == .string);
    try std.testing.expectEqualStrings(file, file_value.string);
    try std.testing.expect(hash == .string);
    try std.testing.expectEqual(@as(usize, 64), hash.string.len);
    try std.testing.expect(refactors == .array);
    try std.testing.expectEqual(expected_count, refactors.array.items.len);
    for (refactors.array.items) |item| {
        try std.testing.expect(item == .object);
        const refactor = item.object;
        try std.testing.expect((refactor.get("kind") orelse return error.MissingKind) == .string);
        try std.testing.expect((refactor.get("line") orelse return error.MissingLine) == .integer);
        try std.testing.expect((refactor.get("column") orelse return error.MissingColumn) == .integer);
        try std.testing.expect((refactor.get("message") orelse return error.MissingMessage) == .string);
        try std.testing.expect((refactor.get("replacement") orelse return error.MissingReplacement) == .string);
    }
    return parsed;
}

fn collectAndWriteJson(source: []const u8) !struct { json: []u8, file: []const u8 } {
    var tmp = std.testing.tmpDir(.{});
    errdefer tmp.cleanup();
    const file = try writeTempHandler(tmp.dir, source);
    errdefer std.testing.allocator.free(file);

    var preview = try collect(std.testing.allocator, file);
    defer preview.deinit(std.testing.allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);
    try writeJson(&aw.writer, &preview);
    buf = aw.toArrayList();

    const json = try buf.toOwnedSlice(std.testing.allocator);
    tmp.cleanup();
    return .{ .json = json, .file = file };
}

test "writeJson envelope is stable with no refactors" {
    const source =
        \\function handler(req: Request): Response {
        \\  return Response.json({ ok: true });
        \\}
    ;
    const out = try collectAndWriteJson(source);
    defer std.testing.allocator.free(out.json);
    defer std.testing.allocator.free(out.file);

    var parsed = try expectCanonicalizeEnvelope(out.json, out.file, 0);
    defer parsed.deinit();
}

test "writeJson envelope covers all deterministic refactor kinds" {
    const cases = [_]struct {
        name: []const u8,
        source: []const u8,
        kind: []const u8,
        replacement: []const u8,
    }{
        .{
            .name = "reused arrow helper",
            .source =
            \\const parse = (x: number): number => x;
            \\function handler(req: Request): Response {
            \\  const a = parse(1);
            \\  const b = parse(2);
            \\  return Response.json({ a, b });
            \\}
            ,
            .kind = "canonicalize_arrow_helper",
            .replacement = "function parse(x: number): number { return x; }",
        },
        .{
            .name = "exported function const",
            .source =
            \\export const load = (id: string): Response => Response.text(id);
            \\function handler(req: Request): Response {
            \\  return load("x");
            \\}
            ,
            .kind = "canonicalize_export_function",
            .replacement = "export function load(id: string): Response { return Response.text(id); }",
        },
        .{
            .name = "avoidable let",
            .source =
            \\function handler(req: Request): Response {
            \\  let count = 1;
            \\  return Response.json({ count });
            \\}
            ,
            .kind = "canonicalize_let_const",
            .replacement = "  const count = 1;",
        },
        .{
            .name = "for-of let",
            .source =
            \\function handler(req: Request): Response {
            \\  const items = [1, 2];
            \\  for (let item of items) {
            \\    Response.json({ item });
            \\  }
            \\  return Response.json({ ok: true });
            \\}
            ,
            .kind = "canonicalize_for_of_const",
            .replacement = "  for (const item of items) {",
        },
        .{
            .name = "capability alias",
            .source =
            \\import { env } from "zigttp:env";
            \\function handler(req: Request): Response {
            \\  let key = "API_KEY";
            \\  const value = env(key);
            \\  return Response.json({ value });
            \\}
            ,
            .kind = "canonicalize_capability_key_alias",
            .replacement = "  const key = \"API_KEY\";",
        },
    };

    for (cases) |case| {
        const out = try collectAndWriteJson(case.source);
        defer std.testing.allocator.free(out.json);
        defer std.testing.allocator.free(out.file);

        var parsed = try expectCanonicalizeEnvelope(out.json, out.file, 1);
        defer parsed.deinit();
        const refactor = parsed.value.object.get("refactors").?.array.items[0].object;
        try std.testing.expectEqualStrings(case.kind, refactor.get("kind").?.string);
        try std.testing.expectEqualStrings(case.replacement, refactor.get("replacement").?.string);
        _ = case.name;
    }
}

test "unsupported typed function-valued const is skipped, not fatal" {
    const source =
        \\type Loader = (id: string) => Response;
        \\export const load: Loader = (id: string): Response => Response.text(id);
        \\function handler(req: Request): Response {
        \\  return load("x");
        \\}
    ;
    const out = try collectAndWriteJson(source);
    defer std.testing.allocator.free(out.json);
    defer std.testing.allocator.free(out.file);

    var parsed = try expectCanonicalizeEnvelope(out.json, out.file, 0);
    defer parsed.deinit();
}

test "generic arrow helper preview is skipped, not malformed" {
    const source =
        \\const id = <T>(x: T): T => x;
        \\function handler(req: Request): Response {
        \\  const a = id(1);
        \\  const b = id(2);
        \\  return Response.json({ a, b });
        \\}
    ;
    const out = try collectAndWriteJson(source);
    defer std.testing.allocator.free(out.json);
    defer std.testing.allocator.free(out.file);

    var parsed = try expectCanonicalizeEnvelope(out.json, out.file, 0);
    defer parsed.deinit();
}

test "dynamic literal-prefix capability alias is not treated as static" {
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req: Request): Response {
        \\  let key = "API_" + req.headers["x"];
        \\  const value = env(key);
        \\  return Response.json({ value });
        \\}
    ;
    const out = try collectAndWriteJson(source);
    defer std.testing.allocator.free(out.json);
    defer std.testing.allocator.free(out.file);

    var parsed = try expectCanonicalizeEnvelope(out.json, out.file, 1);
    defer parsed.deinit();
    const refactor = parsed.value.object.get("refactors").?.array.items[0].object;
    try std.testing.expectEqualStrings("canonicalize_let_const", refactor.get("kind").?.string);
}

test "capability alias preview stays in enclosing scope" {
    const source =
        \\import { env } from "zigttp:env";
        \\function other(req: Request): Response {
        \\  let key = "API_KEY";
        \\  return Response.text(key);
        \\}
        \\function handler(req: Request): Response {
        \\  let key = req.headers["x"];
        \\  const value = env(key);
        \\  return Response.json({ value });
        \\}
    ;
    const out = try collectAndWriteJson(source);
    defer std.testing.allocator.free(out.json);
    defer std.testing.allocator.free(out.file);

    var parsed = try expectCanonicalizeEnvelope(out.json, out.file, 2);
    defer parsed.deinit();
    const refactors = parsed.value.object.get("refactors").?.array.items;
    for (refactors) |item| {
        try std.testing.expect(!std.mem.eql(u8, "canonicalize_capability_key_alias", item.object.get("kind").?.string));
    }
}

test "applyRefactors rejects stale line mismatch" {
    const source =
        \\function handler(req: Request): Response {
        \\  const count = 1;
        \\  return Response.json({ count });
        \\}
    ;
    const refactor = Refactor{
        .kind = "canonicalize_let_const",
        .line = 2,
        .column = 3,
        .message = "let binding is never reassigned",
        .replacement = "  const count = 1;",
        .original_line = "  let count = 1;",
    };
    try std.testing.expectError(error.StaleRefactorLine, applyRefactors(std.testing.allocator, source, &.{refactor}));
}

test "applyRefactors rejects overlapping replacements" {
    const source =
        \\function handler(req: Request): Response {
        \\  let count = 1;
        \\  return Response.json({ count });
        \\}
    ;
    const first = Refactor{
        .kind = "canonicalize_let_const",
        .line = 2,
        .column = 3,
        .message = "let binding is never reassigned",
        .replacement = "  const count = 1;",
        .original_line = "  let count = 1;",
    };
    const second = Refactor{
        .kind = "canonicalize_capability_key_alias",
        .line = 2,
        .column = 1,
        .message = "make capability key alias compiler-visible",
        .replacement = "  const count = 1;",
        .original_line = "  let count = 1;",
    };
    try std.testing.expectError(error.OverlappingRefactors, applyRefactors(std.testing.allocator, source, &.{ first, second }));
}

test "applyRefactors applies multiple lines and edit simulation stays clean" {
    const source =
        \\function handler(req: Request): Response {
        \\  let count = 1;
        \\  const items = [1, 2];
        \\  for (let item of items) {
        \\    Response.json({ item });
        \\  }
        \\  return Response.json({ count });
        \\}
    ;
    const first = Refactor{
        .kind = "canonicalize_let_const",
        .line = 2,
        .column = 3,
        .message = "let binding is never reassigned",
        .replacement = "  const count = 1;",
        .original_line = "  let count = 1;",
    };
    const second = Refactor{
        .kind = "canonicalize_for_of_const",
        .line = 4,
        .column = 3,
        .message = "for-of binding uses let",
        .replacement = "  for (const item of items) {",
        .original_line = "  for (let item of items) {",
    };
    const proposed = try applyRefactors(std.testing.allocator, source, &.{ first, second });
    defer std.testing.allocator.free(proposed);
    try std.testing.expect(std.mem.indexOf(u8, proposed, "let ") == null);

    var simulated = try edit_simulate.simulate(std.testing.allocator, .{
        .file = "handler.ts",
        .content = proposed,
        .before = source,
    });
    defer simulated.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(u32, 0), simulated.new_count);
}

test "invalid applied replacement is caught by edit simulation" {
    const source =
        \\function handler(req: Request): Response {
        \\  let count = 1;
        \\  return Response.json({ count });
        \\}
    ;
    const bad = Refactor{
        .kind = "canonicalize_let_const",
        .line = 2,
        .column = 3,
        .message = "let binding is never reassigned",
        .replacement = "  const = ;",
        .original_line = "  let count = 1;",
    };
    const proposed = try applyRefactors(std.testing.allocator, source, &.{bad});
    defer std.testing.allocator.free(proposed);

    var simulated = try edit_simulate.simulate(std.testing.allocator, .{
        .file = "handler.ts",
        .content = proposed,
        .before = source,
    });
    defer simulated.deinit(std.testing.allocator);
    try std.testing.expect(simulated.new_count > 0);
}

test "collect output can clear canonical diagnostic through edit simulation" {
    const source =
        \\const parse = (x: number): number => x;
        \\function handler(req: Request): Response {
        \\  const a = parse(1);
        \\  const b = parse(2);
        \\  return Response.json({ a, b });
        \\}
    ;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{ .sub_path = "handler.ts", .data = source });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = try tmp.dir.realpath("handler.ts", &path_buf);

    var preview = try collect(std.testing.allocator, abs_path);
    defer preview.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 1), preview.refactors.items.len);
    try std.testing.expectEqualStrings("canonicalize_arrow_helper", preview.refactors.items[0].kind);

    const proposed = try std.fmt.allocPrint(
        std.testing.allocator,
        \\{s}
        \\function handler(req: Request): Response {{
        \\  const a = parse(1);
        \\  const b = parse(2);
        \\  return Response.json({{ a, b }});
        \\}}
    ,
        .{preview.refactors.items[0].replacement},
    );
    defer std.testing.allocator.free(proposed);

    var simulated = try edit_simulate.simulate(std.testing.allocator, .{
        .file = abs_path,
        .content = proposed,
        .before = source,
    });
    defer simulated.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(u32, 0), simulated.total);
}

test "collect output can clear capability alias diagnostic through edit simulation" {
    const source =
        \\import { env } from "zigttp:env";
        \\function handler(req: Request): Response {
        \\  let key = "API_KEY";
        \\  const value = env(key);
        \\  return Response.json({ value });
        \\}
    ;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{ .sub_path = "handler.ts", .data = source });

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = try tmp.dir.realpath("handler.ts", &path_buf);

    var preview = try collect(std.testing.allocator, abs_path);
    defer preview.deinit(std.testing.allocator);
    try std.testing.expect(preview.refactors.items.len >= 1);

    var proposed = try std.ArrayList(u8).initCapacity(std.testing.allocator, source.len + 8);
    defer proposed.deinit(std.testing.allocator);
    try proposed.appendSlice(std.testing.allocator, "import { env } from \"zigttp:env\";\n");
    try proposed.appendSlice(std.testing.allocator, "function handler(req: Request): Response {\n");
    try proposed.appendSlice(std.testing.allocator, preview.refactors.items[0].replacement);
    try proposed.appendSlice(std.testing.allocator, "\n");
    try proposed.appendSlice(std.testing.allocator, "  const value = env(key);\n");
    try proposed.appendSlice(std.testing.allocator, "  return Response.json({ value });\n");
    try proposed.appendSlice(std.testing.allocator, "}\n");

    var simulated = try edit_simulate.simulate(std.testing.allocator, .{
        .file = abs_path,
        .content = proposed.items,
        .before = source,
    });
    defer simulated.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(u32, 0), simulated.total);
}
