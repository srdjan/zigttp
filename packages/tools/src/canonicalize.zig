//! Compiler-authored canonical refactor previews.
//!
//! Emits local rewrite intents only. Callers still apply any replacement
//! through edit-simulate / compiler veto; this module never writes source.

const std = @import("std");
const zigts = @import("zigts");
const precompile = @import("precompile.zig");
const edit_simulate = @import("edit_simulate.zig");
const writeJsonString = zigts.handler_contract.writeJsonString;
const RepairIntent = zigts.repair_intent.RepairIntent;

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
    return collectFromSource(allocator, source, file);
}

/// Like `collect`, but analyzes pre-read `source` against `virtual_path`
/// instead of reading the file from disk. This is the entry point the
/// fixed-point `normalize` loop drives: each pass rewrites source in memory
/// and re-collects without touching the filesystem.
pub fn collectFromSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    virtual_path: []const u8,
) !Result {
    var check = try precompile.runCheckOnlyFromSource(allocator, source, virtual_path, null, true, null, false);
    defer check.deinit(allocator);

    var result = Result{ .file = virtual_path };
    errdefer result.deinit(allocator);

    try buildRefactors(allocator, source, check.json_diagnostics.items, &result);
    return result;
}

/// Translate the diagnostics from one analysis pass into concrete refactors.
/// Shared by `collectFromSource` and the fixed-point `normalizeSource` loop so
/// both read refactors from the same diagnostic set.
fn buildRefactors(
    allocator: std.mem.Allocator,
    source: []const u8,
    diagnostics: []const precompile.json_diag.JsonDiagnostic,
    result: *Result,
) !void {
    for (diagnostics) |diag| {
        const line = sourceLine(source, diag.line) orelse continue;
        if (std.mem.eql(u8, diag.code, "ZTS608") or std.mem.eql(u8, diag.code, "ZTS609")) {
            const replacement = canonicalFunctionReplacement(allocator, line) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, result, .{
                .kind = if (std.mem.eql(u8, diag.code, "ZTS608"))
                    "canonicalize_arrow_helper"
                else
                    "canonicalize_export_function",
                .line = diag.line,
                .column = diag.column,
                .message = try allocator.dupe(u8, diag.message),
                .replacement = replacement,
                .original_line = try allocator.dupe(u8, line),
            });
        } else if (std.mem.eql(u8, diag.code, "ZTS604")) {
            const replacement = avoidableLetReplacement(allocator, line) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, result, .{
                .kind = if (std.mem.indexOf(u8, line, "for (let ") != null)
                    "canonicalize_for_of_const"
                else
                    "canonicalize_let_const",
                .line = diag.line,
                .column = diag.column,
                .message = try allocator.dupe(u8, diag.message),
                .replacement = replacement,
                .original_line = try allocator.dupe(u8, line),
            });
        } else if (std.mem.eql(u8, diag.code, "ZTS613")) {
            const replacement = compoundAssignReplacement(allocator, line) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, result, .{
                .kind = "canonicalize_compound_assign",
                .line = diag.line,
                .column = diag.column,
                .message = try allocator.dupe(u8, diag.message),
                .replacement = replacement,
                .original_line = try allocator.dupe(u8, line),
            });
        } else if (std.mem.eql(u8, diag.code, "ZTS620")) {
            const positive = redundantBoolComparePositive(diag.suggestion) orelse continue;
            const replacement = redundantBoolCompareReplacement(allocator, line, diag.column, positive) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, result, .{
                .kind = "canonicalize_redundant_bool_compare",
                .line = diag.line,
                .column = diag.column,
                .message = try allocator.dupe(u8, diag.message),
                .replacement = replacement,
                .original_line = try allocator.dupe(u8, line),
            });
        } else if (std.mem.eql(u8, diag.code, "ZTS602")) {
            const replacement = capabilityAliasReplacement(allocator, source, diag, diagnostics) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendRefactorUnique(allocator, result, replacement);
        }
    }
}

fn appendRefactorUnique(allocator: std.mem.Allocator, result: *Result, refactor: Refactor) !void {
    var owned = refactor;
    for (result.refactors.items) |*existing| {
        if (existing.line == owned.line and std.mem.eql(u8, existing.replacement, owned.replacement)) {
            if (std.mem.eql(u8, owned.kind, "canonicalize_capability_key_alias")) {
                existing.kind = owned.kind;
                existing.column = owned.column;
                // Move ownership of the duped message: free the old one, take
                // owned's, and clear owned's so freeRefactorOwned skips it.
                if (existing.message.len > 0) allocator.free(existing.message);
                existing.message = owned.message;
                owned.message = "";
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
    // `message` is duped into `allocator` at every build site (ENG-3: it must
    // outlive the CheckResult whose diagnostics it was copied from). The empty
    // string is the freed/moved-out sentinel and is never heap-owned.
    if (refactor.message.len > 0) allocator.free(refactor.message);
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

/// Find the index of the `=>` that is NOT nested inside parens/brackets/angles.
/// Returns the index of `=` in the first outermost `=>`, or null if none found.
fn findOutermostArrow(s: []const u8) ?usize {
    var depth: usize = 0;
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        switch (s[i]) {
            '(', '[', '<' => depth += 1,
            ')', ']', '>' => if (depth > 0) { depth -= 1; },
            '=' => if (depth == 0 and i + 1 < s.len and s[i + 1] == '>') return i,
            else => {},
        }
    }
    return null;
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
    // Find the outermost `=>` — skip any `=>` that appears inside nested
    // parentheses, brackets, or angle brackets (e.g. parameter type annotations
    // like `(fn: (x: number) => string) => body`).
    const arrow = findOutermostArrow(arrow_source) orelse return error.UnsupportedRefactor;
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

        // Separate the expression from a trailing ';' so we parenthesize only
        // the expression. Refuse anything with an interior ';' or a comment -
        // those defeat the simple textual split and must be edited by hand.
        const rhs_trimmed = trimRight(rhs, " \t");
        const has_semi = rhs_trimmed.len > 0 and rhs_trimmed[rhs_trimmed.len - 1] == ';';
        const expr = if (has_semi) trimRight(rhs_trimmed[0 .. rhs_trimmed.len - 1], " \t") else rhs_trimmed;
        const trailer: []const u8 = if (has_semi) ";" else "";
        if (expr.len == 0) return error.UnsupportedRefactor;
        if (std.mem.indexOfScalar(u8, expr, ';') != null) return error.UnsupportedRefactor;
        if (std.mem.indexOf(u8, expr, "//") != null or std.mem.indexOf(u8, expr, "/*") != null) {
            return error.UnsupportedRefactor;
        }

        // A single atom (identifier, number, dotted chain) needs no parens, so
        // `n += 1` stays `n = n + 1`. Any compound expression MUST be
        // parenthesized: `total -= fee + tax` becomes `total = total - (fee +
        // tax)`, not `total = total - fee + tax` which reassociates to
        // `(total - fee) + tax` and silently changes the result.
        if (isAtomicRhs(expr)) {
            return std.fmt.allocPrint(allocator, "{s}{s} = {s} {c} {s}{s}", .{
                indent, lhs, lhs, op_char, expr, trailer,
            });
        }
        if (!delimitersBalanced(expr)) return error.UnsupportedRefactor;
        return std.fmt.allocPrint(allocator, "{s}{s} = {s} {c} ({s}){s}", .{
            indent, lhs, lhs, op_char, expr, trailer,
        });
    }
    return error.UnsupportedRefactor;
}

fn isArithmeticOpChar(c: u8) bool {
    return c == '+' or c == '-' or c == '*' or c == '/' or c == '%';
}

/// True when `expr` is a single atom needing no parentheses on the right of a
/// rewritten compound assignment: an identifier, number, or dotted chain. Any
/// expression containing a top-level operator, call, index, or string literal
/// returns false and is parenthesized to preserve operator precedence.
fn isAtomicRhs(expr: []const u8) bool {
    if (expr.len == 0) return false;
    for (expr) |c| {
        if (!(std.ascii.isAlphanumeric(c) or c == '_' or c == '.' or c == '$')) return false;
    }
    return true;
}

/// ZTS620: rewrite `x === true` / `x !== false` -> `x` and `x === false`
/// / `x !== true` -> `!x` (and the operand-reversed forms), in place on the
/// line that carries the comparison. The strict checker only fires for a
/// statically-boolean value operand, so dropping the literal comparison is
/// behavior-preserving: for a boolean `x`, `x === true` is exactly `x`.
///
/// `column` is the 1-based column at which the comparison *operator*
/// (`===`/`!==`) starts on `line` -- the location `IrView.getLoc` reports for
/// a binary-expression node. `positive` selects the `x` form (`=== true` /
/// `!== false`) over the negated `!x` form; the caller reads it from the
/// diagnostic help text.
///
/// Conservative by design: the *value* operand must be a simple lvalue (an
/// identifier or dotted-identifier chain such as `a` or `a.b.c`). A value
/// operand containing a call, index, or operator is refused with
/// `UnsupportedRefactor` so the comparison stays a flagged hard error rather
/// than risk a mis-parsed span or an unparenthesised `!`-precedence change.
pub fn redundantBoolCompareReplacement(
    allocator: std.mem.Allocator,
    line: []const u8,
    column: u16,
    positive: bool,
) ![]u8 {
    if (column == 0) return error.UnsupportedRefactor;
    const op_start: usize = @as(usize, column) - 1;
    if (op_start + 3 > line.len) return error.UnsupportedRefactor;

    // Operator: strict equality or strict inequality at the diagnostic column.
    const op = line[op_start .. op_start + 3];
    if (!std.mem.eql(u8, op, "===") and !std.mem.eql(u8, op, "!==")) return error.UnsupportedRefactor;

    // Left operand: scan backwards over whitespace, then over an operand token.
    const left_ws_end = trimSpacesBack(line, op_start);
    const left_start = scanOperandTokenBack(line, left_ws_end) orelse return error.UnsupportedRefactor;
    const left_tok = line[left_start..left_ws_end];

    // Right operand: scan forwards over whitespace, then over an operand token.
    const right_start = skipSpaces(line, op_start + 3);
    const right_tok = scanOperandToken(line, right_start) orelse return error.UnsupportedRefactor;
    const right_end = right_start + right_tok.len;

    const left_is_literal = isBoolLiteralToken(left_tok);
    const right_is_literal = isBoolLiteralToken(right_tok);
    // Exactly one side is the boolean literal (the strict checker's invariant).
    if (left_is_literal == right_is_literal) return error.UnsupportedRefactor;

    const value = if (left_is_literal) right_tok else left_tok;
    if (!isSimpleLvalue(value)) return error.UnsupportedRefactor;

    // The comparison span on the line is [left_start, right_end). Replace it
    // with the bare boolean (`x`) or its negation (`!x`), preserving everything
    // around it.
    const replacement_core = if (positive)
        try allocator.dupe(u8, value)
    else
        try std.fmt.allocPrint(allocator, "!{s}", .{value});
    defer allocator.free(replacement_core);

    return std.fmt.allocPrint(allocator, "{s}{s}{s}", .{
        line[0..left_start],
        replacement_core,
        line[right_end..],
    });
}

/// Scan a single operand token starting at `pos`: either a boolean literal
/// (`true`/`false`) or a simple-lvalue chain (`a`, `a.b.c`). Returns the token
/// slice, or null when `pos` does not begin one. Used to bound the operand
/// spans of a ZTS620 comparison without a full expression parser.
fn scanOperandToken(line: []const u8, pos: usize) ?[]const u8 {
    if (pos >= line.len or !isIdentStart(line[pos])) return null;
    var end = pos + 1;
    while (end < line.len) {
        const c = line[end];
        if (isIdentContinue(c)) {
            end += 1;
            continue;
        }
        if (c == '.' and end + 1 < line.len and isIdentStart(line[end + 1])) {
            end += 1;
            continue;
        }
        break;
    }
    return line[pos..end];
}

/// Find the start index of the operand token whose last character is at
/// `end - 1` (exclusive `end`), walking back over ident-continue characters and
/// dotted-chain separators. Returns null when `[?..end)` is not a valid simple
/// operand (e.g. it is preceded by a `)` or `]`, signalling a call/index whose
/// span this line-local scan cannot bound safely).
fn scanOperandTokenBack(line: []const u8, end: usize) ?usize {
    if (end == 0) return null;
    var start = end;
    while (start > 0) {
        const c = line[start - 1];
        if (isIdentContinue(c)) {
            start -= 1;
            continue;
        }
        // A dot continues a member chain only when an identifier sits on both
        // sides of it (`a.b`), never a leading dot or `).foo`.
        if (c == '.' and start >= 2 and (isIdentContinue(line[start - 2]))) {
            start -= 1;
            continue;
        }
        break;
    }
    if (start == end) return null;
    if (!isIdentStart(line[start])) return null;
    // Refuse a token that is the tail of a larger member/call/index expression
    // the line-local scan cannot bound (`obj().foo`, `a[i].foo`): the `.` before
    // `start` was not consumed because its left side was `)`/`]`. Rewriting only
    // the tail would splice `!foo` after `obj().`, producing `obj().!foo`.
    if (start > 0) {
        const before = line[start - 1];
        if (before == '.' or before == ')' or before == ']') return null;
    }
    return start;
}

/// Trim trailing spaces/tabs from `[0..end)`, returning the new exclusive end.
fn trimSpacesBack(line: []const u8, end: usize) usize {
    var e = end;
    while (e > 0 and (line[e - 1] == ' ' or line[e - 1] == '\t')) e -= 1;
    return e;
}

fn isBoolLiteralToken(tok: []const u8) bool {
    return std.mem.eql(u8, tok, "true") or std.mem.eql(u8, tok, "false");
}

fn skipSpaces(line: []const u8, pos: usize) usize {
    var i = pos;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) i += 1;
    return i;
}

/// Read the positive/negated form of a ZTS620 rewrite from the diagnostic help
/// text. The strict checker emits `... use the boolean directly: \`x\`` for the
/// `x` form and `... negate the boolean directly: \`!x\`` for the `!x` form.
fn redundantBoolComparePositive(help: ?[]const u8) ?bool {
    const text = help orelse return null;
    if (std.mem.indexOf(u8, text, "use the boolean directly") != null) return true;
    if (std.mem.indexOf(u8, text, "negate the boolean directly") != null) return false;
    return null;
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
        .message = try allocator.dupe(u8, "make capability key alias compiler-visible"),
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

// ---------------------------------------------------------------------------
// Span-keyed (multi-line) rewrites
// ---------------------------------------------------------------------------
//
// `applyRefactors` above is line-keyed: it rejects any replacement spanning
// more than one line and any two refactors that touch the same line. The
// Phase-1 single-line rewriters depend on those guards, so they are left
// untouched. The convention / multi-line canonical rewrites (ternary ->
// match, ...) instead key on a byte span `[start_offset, end_offset)` of the
// source and may produce multi-line output. `applyStatementRewrites` is the
// second, span-keyed application path that serves them. It is kept entirely
// separate from `applyRefactors`: the normalize loop drives one or the other
// per pass, never both at once.

pub const StatementRewrite = struct {
    /// The typed intent this rewrite realizes (recorded in the rewrite trace).
    intent: RepairIntent,
    /// Byte span of the construct being replaced, in the source it was
    /// computed against. Half-open: `[start_offset, end_offset)`.
    start_offset: usize,
    end_offset: usize,
    /// The replacement text (may contain newlines).
    replacement: []u8,
    /// A snapshot of `source[start_offset..end_offset]` at the time the
    /// rewrite was built. `applyStatementRewrites` re-validates the span still
    /// matches before splicing, so a stale rewrite fails closed rather than
    /// corrupting unrelated source.
    original: []u8,

    pub fn deinit(self: *StatementRewrite, allocator: std.mem.Allocator) void {
        allocator.free(self.replacement);
        allocator.free(self.original);
        self.* = undefined;
    }
};

/// Convert a 1-based (line, column) byte position into a byte offset into
/// `source`. Returns null when the position is out of range. Column is a byte
/// column (the tokenizer computes it as `pos - line_start + 1`), and the
/// TypeScript stripper preserves byte positions by blanking stripped spans
/// with spaces, so a diagnostic column maps to the same byte offset in the
/// original source the rewriter scans.
fn lineColToOffset(source: []const u8, line: u32, column: u16) ?usize {
    if (line == 0 or column == 0) return null;
    var current: u32 = 1;
    var line_start: usize = 0;
    var i: usize = 0;
    while (current < line and i < source.len) : (i += 1) {
        if (source[i] == '\n') {
            current += 1;
            line_start = i + 1;
        }
    }
    if (current != line) return null;
    const offset = line_start + (@as(usize, column) - 1);
    if (offset > source.len) return null;
    return offset;
}

/// Apply a set of byte-span rewrites to `source`, returning fresh owned
/// output. The set must be NON-OVERLAPPING: any two spans that overlap are a
/// caller bug (the normalize loop only ever passes the innermost
/// non-overlapping subset per pass), and are rejected with
/// `error.OverlappingRefactors`. Each rewrite's `original` snapshot must still
/// match `source[start..end)`, else `error.StaleRefactorLine`. Spans are
/// spliced right-to-left so an earlier splice never shifts a later span's
/// offsets.
pub fn applyStatementRewrites(
    allocator: std.mem.Allocator,
    source: []const u8,
    rewrites: []const StatementRewrite,
) ![]u8 {
    if (rewrites.len == 0) return allocator.dupe(u8, source);

    // Order indices by ascending start offset so we can both detect overlaps
    // and splice deterministically. A bounded insertion sort over a small set.
    const order = try allocator.alloc(usize, rewrites.len);
    defer allocator.free(order);
    for (order, 0..) |*o, i| o.* = i;
    for (1..order.len) |i| {
        var j = i;
        while (j > 0 and rewrites[order[j - 1]].start_offset > rewrites[order[j]].start_offset) : (j -= 1) {
            const tmp = order[j - 1];
            order[j - 1] = order[j];
            order[j] = tmp;
        }
    }

    // Validate bounds, overlap, and the original snapshot for each span.
    var prev_end: ?usize = null;
    for (order) |idx| {
        const rw = rewrites[idx];
        if (rw.start_offset > rw.end_offset or rw.end_offset > source.len) return error.RefactorLineNotFound;
        if (prev_end) |pe| {
            if (rw.start_offset < pe) return error.OverlappingRefactors;
        }
        if (!std.mem.eql(u8, source[rw.start_offset..rw.end_offset], rw.original)) return error.StaleRefactorLine;
        prev_end = rw.end_offset;
    }

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    var cursor: usize = 0;
    for (order) |idx| {
        const rw = rewrites[idx];
        try out.appendSlice(allocator, source[cursor..rw.start_offset]);
        try out.appendSlice(allocator, rw.replacement);
        cursor = rw.end_offset;
    }
    try out.appendSlice(allocator, source[cursor..]);
    return try out.toOwnedSlice(allocator);
}

// ---------------------------------------------------------------------------
// Statement-rewrite construction (from diagnostics)
// ---------------------------------------------------------------------------

/// Owned list of span-keyed rewrites built for one analysis pass. Distinct
/// from `Result` (which carries the single-line `Refactor`s) so the two
/// application paths never share storage or guards.
pub const StatementRewriteResult = struct {
    rewrites: std.ArrayListUnmanaged(StatementRewrite) = .empty,

    pub fn deinit(self: *StatementRewriteResult, allocator: std.mem.Allocator) void {
        for (self.rewrites.items) |*rw| rw.deinit(allocator);
        self.rewrites.deinit(allocator);
        self.* = .{};
    }
};

/// Translate the diagnostics from one analysis pass into span-keyed
/// rewrites. The sibling of `buildRefactors` for the multi-line / convention
/// rules: each landed rule dispatches on `diag.code` to a builder that derives
/// the construct's byte span from the source and the diagnostic's start
/// position, returning `error.UnsupportedRefactor` to leave the construct a
/// flagged hard error when a provably-safe rewrite cannot be formed.
fn buildStatementRewrites(
    allocator: std.mem.Allocator,
    source: []const u8,
    diagnostics: []const precompile.json_diag.JsonDiagnostic,
    result: *StatementRewriteResult,
) !void {
    for (diagnostics) |diag| {
        if (std.mem.eql(u8, diag.code, "ZTS612")) {
            const rw = ternaryToMatchRewrite(allocator, source, diag.line, diag.column) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendStatementRewriteUnique(allocator, result, rw);
        } else if (std.mem.eql(u8, diag.code, "ZTS615")) {
            const rw = templateHoistRewrite(allocator, source, diag.line, diag.column) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendStatementRewriteUnique(allocator, result, rw);
        } else if (std.mem.eql(u8, diag.code, "ZTS617")) {
            const rw = defaultParamLiftRewrite(allocator, source, diag.line, diag.column) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendStatementRewriteUnique(allocator, result, rw);
        } else if (std.mem.eql(u8, diag.code, "ZTS618")) {
            const rw = nestedDestructureRewrite(allocator, source, diag.line, diag.column) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendStatementRewriteUnique(allocator, result, rw);
        } else if (std.mem.eql(u8, diag.code, "ZTS619")) {
            const rw = unusedIndexAliasRewrite(allocator, source, diag.line, diag.column) catch |err| switch (err) {
                error.UnsupportedRefactor => continue,
                else => return err,
            };
            try appendStatementRewriteUnique(allocator, result, rw);
        }
    }
}

/// ZTS612 canonical_ternary: rewrite `cond ? a : b` into an expression-position
/// `match` so it works in const initializers and return positions, where an
/// if/else statement cannot.
///
/// Canonical target: `match (!!(cond)) { when true: a, default: b }` (the
/// condition is parenthesized so `!!` coerces the whole expression).
///
/// The double-negation `!!cond` is load-bearing for behavior preservation, not
/// cosmetic. A ternary tests the *truthiness* of `cond` (the VM's `if_false`
/// coerces with `toConditionBool`), whereas a `match` boolean-literal arm tests
/// *strict equality* against `true` (`strict_eq`). For a `cond` that is truthy
/// but not the literal `true` (e.g. a non-empty string or non-zero number),
/// `cond ? a : b` yields `a` while `match (cond) { when true: a, default: b }`
/// would fall to `default`. `!` also coerces with `toConditionBool` and raises
/// the same bool-error on the same un-coercible values, so `!!cond` reproduces
/// the ternary's branch selection exactly for every `cond`. Both forms evaluate
/// `cond` once.
///
/// `column` is the 1-based column of the `?` token (the location the strict
/// checker reports for a ternary node). From there the condition span is found
/// by scanning back to the nearest enclosing expression boundary, and the
/// then/else spans by scanning forward across the matching `:` and to the end
/// of the else branch. Any span that cannot be bounded with the conservative
/// scanner (an unbalanced bracket, a boundary the scanner does not recognise)
/// is refused with `error.UnsupportedRefactor`, leaving the ternary a flagged
/// hard error rather than risking a behavior-changing splice.
fn ternaryToMatchRewrite(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
    column: u16,
) !StatementRewrite {
    const q = lineColToOffset(source, line, column) orelse return error.UnsupportedRefactor;
    if (q >= source.len or source[q] != '?') return error.UnsupportedRefactor;
    // `?.` (optional chain) and `??` (nullish) are not ternaries.
    if (q + 1 < source.len and (source[q + 1] == '.' or source[q + 1] == '?')) return error.UnsupportedRefactor;

    var cond_start = ternaryConditionStart(source, q) orelse return error.UnsupportedRefactor;
    // A `return <ternary>` statement: the backward scan stops at the line/source
    // boundary and sweeps the `return` keyword into the condition. Step past a
    // leading `return ` so the condition is the bare boolean expression.
    cond_start = skipLeadingReturn(source, cond_start, q);
    const colon = ternaryMatchingColon(source, q + 1) orelse return error.UnsupportedRefactor;
    const else_end = ternaryElseEnd(source, colon + 1) orelse return error.UnsupportedRefactor;

    const cond = std.mem.trim(u8, source[cond_start..q], " \t");
    const then_branch = std.mem.trim(u8, source[q + 1 .. colon], " \t");
    const else_branch = std.mem.trim(u8, source[colon + 1 .. else_end], " \t");
    if (cond.len == 0 or then_branch.len == 0 or else_branch.len == 0) return error.UnsupportedRefactor;

    // The spliced span begins at the first non-space byte of the condition so
    // the surrounding indentation/operators are preserved verbatim.
    const real_start = cond_start + leadingSpaces(source[cond_start..q]);

    // Parenthesize the condition: `!!` is unary and binds tighter than the
    // relational/equality operators a condition usually ends in, so `!!a === b`
    // would mis-parse as `(!!a) === b`. `!!(a === b)` coerces the whole
    // condition to a strict boolean, reproducing the ternary's truthiness test.
    const replacement = try std.fmt.allocPrint(
        allocator,
        "match (!!({s})) {{ when true: {s}, default: {s} }}",
        .{ cond, then_branch, else_branch },
    );
    errdefer allocator.free(replacement);
    const original = try allocator.dupe(u8, source[real_start..else_end]);
    errdefer allocator.free(original);

    return .{
        .intent = .replace_ternary_with_if,
        .start_offset = real_start,
        .end_offset = else_end,
        .replacement = replacement,
        .original = original,
    };
}

fn leadingSpaces(s: []const u8) usize {
    var i: usize = 0;
    while (i < s.len and (s[i] == ' ' or s[i] == '\t')) i += 1;
    return i;
}

/// If `source[start..end)` (after leading whitespace) begins with the `return`
/// keyword followed by whitespace, return the offset just past that whitespace
/// so the ternary condition excludes the keyword. Otherwise return `start`
/// unchanged.
fn skipLeadingReturn(source: []const u8, start: usize, end: usize) usize {
    var i = start + leadingSpaces(source[start..end]);
    const kw = "return";
    if (i + kw.len <= end and std.mem.eql(u8, source[i .. i + kw.len], kw)) {
        const after = i + kw.len;
        if (after < end and (source[after] == ' ' or source[after] == '\t')) {
            i = after;
            while (i < end and (source[i] == ' ' or source[i] == '\t')) i += 1;
            return i;
        }
    }
    return start;
}

/// Find the start offset of a ternary condition, scanning back from the `?` at
/// `q`. The condition runs back to the nearest enclosing expression boundary at
/// bracket depth zero: an assignment `=` (not `==`/`=>`/`<=`/`>=`/`!=`), an
/// open bracket `([{`, a comma, a semicolon, a `return`/`=>` keyword boundary,
/// or the start of the line/source. Brackets and string/template literals are
/// balanced so a boundary character inside them does not terminate the scan.
/// Returns null when the scan cannot find a clean boundary.
fn ternaryConditionStart(source: []const u8, q: usize) ?usize {
    var i: usize = q;
    var depth: i32 = 0;
    while (i > 0) {
        const c = source[i - 1];
        switch (c) {
            ')', ']', '}' => {
                depth += 1;
                i -= 1;
            },
            '(', '[', '{' => {
                if (depth == 0) return i; // boundary: opening bracket
                depth -= 1;
                i -= 1;
            },
            ',', ';' => {
                if (depth == 0) return i;
                i -= 1;
            },
            ':', '?' => {
                // `c == source[i-1]`, so returning `i` starts the condition
                // just after this boundary. A depth-0 `:` belongs to an
                // enclosing ternary (or an object key / label); a depth-0 `?`
                // is an enclosing ternary's `?` whose else-branch contains this
                // one (right-associative chains). The condition of *this*
                // ternary cannot extend back past either.
                if (depth == 0) return i;
                i -= 1;
            },
            '\n' => {
                if (depth == 0) return i;
                i -= 1;
            },
            '"', '\'', '`' => {
                // Skip back over a string/template literal.
                const lit_start = scanStringBack(source, i - 1, c) orelse return null;
                i = lit_start;
            },
            '=' => {
                if (depth != 0) {
                    i -= 1;
                    continue;
                }
                // Distinguish a plain assignment `=` (boundary) from a
                // comparison/arrow operator that is part of the condition.
                const prev: u8 = if (i >= 2) source[i - 2] else 0;
                const next: u8 = if (i < source.len) source[i] else 0;
                const is_compare = prev == '=' or prev == '!' or prev == '<' or prev == '>';
                const is_arrow = next == '>';
                const is_eqeq = next == '=';
                if (is_arrow) {
                    // `=>` is a boundary: the ternary condition is the arrow's
                    // expression body and starts just after the `>` (the `=` is
                    // at i-1, the `>` at i). Without this, `(x) => cond ? a : b`
                    // would sweep `(x) => cond` into the condition, yielding an
                    // always-truthy `match (!!((x) => cond))`.
                    return i + 1;
                }
                if (is_compare or is_eqeq) {
                    i -= 1;
                    continue;
                }
                return i; // plain assignment: condition begins after the `=`
            },
            else => i -= 1,
        }
    }
    if (depth == 0) return 0;
    return null;
}

/// From `start` (the offset just past the `?`), find the offset of the `:`
/// that closes this ternary. Right-associative: the matching colon is the
/// first `:` at the same nesting depth, but a *nested* ternary consumes its own
/// colon, so an inner `?` increments a pending-colon counter. Brackets and
/// string/template literals are balanced. Returns null when no matching colon
/// is found.
fn ternaryMatchingColon(source: []const u8, start: usize) ?usize {
    var i: usize = start;
    var depth: i32 = 0;
    var pending: u32 = 0; // nested ternaries awaiting their own colon
    while (i < source.len) {
        const c = source[i];
        switch (c) {
            '(', '[', '{' => {
                depth += 1;
                i += 1;
            },
            ')', ']', '}' => {
                if (depth == 0) return null;
                depth -= 1;
                i += 1;
            },
            '"', '\'', '`' => {
                const after = scanStringForward(source, i, c) orelse return null;
                i = after;
            },
            '?' => {
                // `?.` and `??` are not nested ternaries.
                if (i + 1 < source.len and (source[i + 1] == '.' or source[i + 1] == '?')) {
                    i += 2;
                    continue;
                }
                if (depth == 0) pending += 1;
                i += 1;
            },
            ':' => {
                if (depth == 0) {
                    if (pending == 0) return i;
                    pending -= 1;
                }
                i += 1;
            },
            ';', '\n' => {
                if (depth == 0) return null; // unterminated on this statement
                i += 1;
            },
            else => i += 1,
        }
    }
    return null;
}

/// From `start` (just past the closing `:`), find the exclusive end of the
/// else branch: the first boundary at depth zero (`,` `;` `)` `]` `}` newline,
/// or end of source). Brackets and string/template literals are balanced.
fn ternaryElseEnd(source: []const u8, start: usize) ?usize {
    var i: usize = start;
    var depth: i32 = 0;
    while (i < source.len) {
        const c = source[i];
        switch (c) {
            '(', '[', '{' => {
                depth += 1;
                i += 1;
            },
            ')', ']', '}' => {
                if (depth == 0) return i;
                depth -= 1;
                i += 1;
            },
            '"', '\'', '`' => {
                const after = scanStringForward(source, i, c) orelse return null;
                i = after;
            },
            ',', ';', '\n' => {
                if (depth == 0) return i;
                i += 1;
            },
            else => i += 1,
        }
    }
    return i;
}

/// Scan forward over a string or template literal that opens at `open`
/// (`source[open] == quote`). Returns the offset just past the closing quote,
/// or null when unterminated. Template literals (`` ` ``) only track escapes,
/// not nested `${...}` interpolations: a `${...}` cannot contain an unbalanced
/// closing backtick, so for the bracket/colon scans above treating the whole
/// template as opaque is sufficient and conservative.
fn scanStringForward(source: []const u8, open: usize, quote: u8) ?usize {
    var i: usize = open + 1;
    var escaped = false;
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
    return null;
}

/// Scan backward over a string or template literal whose closing quote is at
/// `close` (`source[close] == quote`). Returns the offset of the opening quote,
/// or null when the literal cannot be bounded (an escaped opener mid-scan makes
/// a backward scan ambiguous, so refuse). Conservative: any backslash before
/// the matching opener aborts the scan.
fn scanStringBack(source: []const u8, close: usize, quote: u8) ?usize {
    var i: usize = close;
    while (i > 0) {
        i -= 1;
        if (source[i] == quote) {
            // Count preceding backslashes; an odd count means this quote is
            // escaped and is not the opener. A backward scan cannot resolve
            // that cheaply, so refuse.
            var bs: usize = 0;
            var j = i;
            while (j > 0 and source[j - 1] == '\\') : (j -= 1) bs += 1;
            if (bs % 2 == 0) return i;
            return null;
        }
        if (source[i] == '\n') return null;
    }
    return null;
}

/// ZTS615 canonical_template_complex_interp: hoist each complex `${expr}` in a
/// template literal into a `const` immediately above the statement, then
/// interpolate the new name. The canonical rule wants every template
/// interpolation to be a bare identifier or a literal-keyed property access
/// (`isSimpleTemplateInterp`); anything else (a call, an index, an operator
/// expression) is hoisted.
///
/// Canonical target, for `  const g = ` + "`" + `Hi ${u.up()}` + "`" + `;`:
///     const __zt_<off> = u.up();
///     const g = ` + "`" + `Hi ${__zt_<off>}` + "`" + `;
///
/// The generated binding name `__zt_<off>` is derived from the byte offset of
/// the interpolation's opening `${`, never a mutable counter, so the normal
/// form is unique and confluent: the same input always yields the same name.
///
/// Behavior preservation: template interpolations evaluate left-to-right at the
/// point the template is evaluated. Hoisting each complex interp, in source
/// order, to a `const` on the line directly above preserves that order and the
/// single evaluation point. The hoisted expression sits in statement position
/// where its value is identical to its value in interpolation position.
///
/// Scope is deliberately narrow for provable safety: the entire enclosing
/// statement must be on ONE physical line (`column` reports the template
/// literal start; a multi-line template or statement is refused so the
/// line-local splice never straddles a construct it cannot see). Any interp
/// whose `${...}` cannot be balanced is refused, leaving ZTS615 a flagged hard
/// error. A template inside a function that opens on the same line (an arrow
/// or a `function` keyword before the backtick) is also refused: the hoisted
/// `const` would land outside that function, capturing the wrong binding for
/// any parameter or local the interpolation references and moving evaluation
/// from call time to definition time.
fn templateHoistRewrite(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
    column: u16,
) !StatementRewrite {
    const tmpl_start = lineColToOffset(source, line, column) orelse return error.UnsupportedRefactor;
    if (tmpl_start >= source.len or source[tmpl_start] != '`') return error.UnsupportedRefactor;

    // The enclosing statement must be a single physical line: bound it by the
    // surrounding newlines.
    const line_start = lineStartOffset(source, tmpl_start);
    const line_end = std.mem.indexOfScalarPos(u8, source, tmpl_start, '\n') orelse source.len;
    const stmt = source[line_start..line_end];

    // The hoisted `const` is emitted at line_start, so a statement preceding the
    // template on the SAME physical line would have its side effects reordered
    // after the hoist. Refuse when anything but a binding/return precedes the
    // template: a `;` in the prefix signals a prior statement on the line.
    if (std.mem.indexOfScalar(u8, source[line_start..tmpl_start], ';') != null) {
        return error.UnsupportedRefactor;
    }

    // A function opening before the template on this line would put the
    // hoisted `const` outside that function's scope.
    if (prefixOpensFunctionScope(source[line_start..tmpl_start])) {
        return error.UnsupportedRefactor;
    }

    // Find the matching closing backtick of this template, staying on one line.
    const tmpl_close = templateCloseOffset(source, tmpl_start, line_end) orelse return error.UnsupportedRefactor;

    // Collect every complex `${...}` in this template, in source order.
    var hoists: std.ArrayListUnmanaged(struct { expr_start: usize, expr_end: usize, name: []u8 }) = .empty;
    defer {
        for (hoists.items) |h| allocator.free(h.name);
        hoists.deinit(allocator);
    }

    var i: usize = tmpl_start + 1;
    while (i < tmpl_close) {
        if (source[i] == '\\') {
            i += 2;
            continue;
        }
        if (source[i] == '$' and i + 1 < tmpl_close and source[i + 1] == '{') {
            const expr_start = i + 2;
            const close = matchingBrace(source, expr_start, tmpl_close) orelse return error.UnsupportedRefactor;
            const inner = std.mem.trim(u8, source[expr_start..close], " \t");
            if (inner.len == 0) return error.UnsupportedRefactor;
            if (!isSimpleTemplateInterpText(inner)) {
                const name = try std.fmt.allocPrint(allocator, "__zt_{d}", .{i});
                errdefer allocator.free(name);
                try hoists.append(allocator, .{ .expr_start = expr_start, .expr_end = close, .name = name });
            }
            i = close + 1;
            continue;
        }
        i += 1;
    }

    if (hoists.items.len == 0) return error.UnsupportedRefactor;

    const indent = stmt[0..leadingSpaces(stmt)];

    // Build the replacement: one hoist `const` per complex interp (in order),
    // then the original statement line with each `${expr}` replaced by
    // `${name}`.
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);
    for (hoists.items) |h| {
        try out.appendSlice(allocator, indent);
        try out.appendSlice(allocator, "const ");
        try out.appendSlice(allocator, h.name);
        try out.appendSlice(allocator, " = ");
        try out.appendSlice(allocator, std.mem.trim(u8, source[h.expr_start..h.expr_end], " \t"));
        try out.appendSlice(allocator, ";\n");
    }
    // Emit the statement with interps replaced.
    var cursor = line_start;
    for (hoists.items) |h| {
        try out.appendSlice(allocator, source[cursor..h.expr_start]);
        try out.appendSlice(allocator, h.name);
        cursor = h.expr_end;
    }
    try out.appendSlice(allocator, source[cursor..line_end]);

    const replacement = try out.toOwnedSlice(allocator);
    errdefer allocator.free(replacement);
    const original = try allocator.dupe(u8, source[line_start..line_end]);
    errdefer allocator.free(original);

    return .{
        .intent = .name_const_above_template,
        .start_offset = line_start,
        .end_offset = line_end,
        .replacement = replacement,
        .original = original,
    };
}

/// Offset of the start of the line containing `pos` (the byte just after the
/// previous newline, or 0).
fn lineStartOffset(source: []const u8, pos: usize) usize {
    var i = pos;
    while (i > 0 and source[i - 1] != '\n') i -= 1;
    return i;
}

/// Find the closing backtick of a template literal that opens at `open`,
/// scanning forward but not past `limit`. `${...}` interpolations are skipped
/// with brace balancing (an interp may contain a backtick inside a nested
/// string or template). Returns null when the template does not close before
/// `limit` (e.g. a multi-line template, which this rewriter refuses).
fn templateCloseOffset(source: []const u8, open: usize, limit: usize) ?usize {
    var i: usize = open + 1;
    while (i < limit) {
        const c = source[i];
        if (c == '\\') {
            i += 2;
            continue;
        }
        if (c == '`') return i;
        if (c == '$' and i + 1 < limit and source[i + 1] == '{') {
            const close = matchingBrace(source, i + 2, limit) orelse return null;
            i = close + 1;
            continue;
        }
        i += 1;
    }
    return null;
}

/// Given `start` just past a `${`, return the offset of the matching `}`,
/// balancing nested braces and skipping string/template literals. Bounded by
/// `limit`. Returns null when no match is found before `limit`.
fn matchingBrace(source: []const u8, start: usize, limit: usize) ?usize {
    var i: usize = start;
    var depth: i32 = 0;
    while (i < limit) {
        const c = source[i];
        switch (c) {
            '{' => {
                depth += 1;
                i += 1;
            },
            '}' => {
                if (depth == 0) return i;
                depth -= 1;
                i += 1;
            },
            '"', '\'', '`' => {
                const after = scanStringForward(source, i, c) orelse return null;
                if (after > limit) return null;
                i = after;
            },
            else => i += 1,
        }
    }
    return null;
}

/// Mirror of the strict checker's `isSimpleTemplateInterp`, but over text: a
/// simple interpolation is a bare identifier or a dotted-identifier chain
/// (`a`, `a.b.c`). Anything else (a call, an index, an operator, whitespace
/// between tokens) is complex and gets hoisted. Kept conservative: when in
/// doubt the text is treated as complex (hoisted), never as simple.
fn isSimpleTemplateInterpText(s: []const u8) bool {
    return isSimpleLvalue(s);
}

/// True when `prefix` (the statement-line text before the template literal)
/// opens a function scope: an `=>` arrow or a word-bounded `function` keyword.
/// Purely textual and deliberately over-broad (a `=>` inside a string in the
/// prefix also matches): a false positive only suppresses the auto-fix and
/// leaves ZTS615 a flagged hard error, never produces a wrong rewrite.
fn prefixOpensFunctionScope(prefix: []const u8) bool {
    if (std.mem.indexOf(u8, prefix, "=>") != null) return true;
    var i: usize = 0;
    while (std.mem.indexOfPos(u8, prefix, i, "function")) |at| {
        const before_ok = at == 0 or !isIdentContinue(prefix[at - 1]);
        const after = at + "function".len;
        const after_ok = after >= prefix.len or !isIdentContinue(prefix[after]);
        if (before_ok and after_ok) return true;
        i = at + 1;
    }
    return false;
}

/// ZTS617 canonical_default_parameter: remove a simple signature-site default
/// and make the fallback a first statement in the function body.
///
/// Supported shape is deliberately narrow:
///   `function f(name: T = expr): R {`
///   `function f(name = expr) {`
///
/// The opening brace must end the physical line so inserting the guard cannot
/// reorder an existing same-line statement. The parameter must be a simple
/// identifier. When an explicit type annotation is present, the type is widened
/// to include `undefined`. The body starts with `if (name === undefined) {
/// name = expr; }`, preserving the runtime behavior of omitted/undefined
/// arguments while moving the default into visible statement position.
fn defaultParamLiftRewrite(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
    column: u16,
) !StatementRewrite {
    const param_pos = lineColToOffset(source, line, column) orelse return error.UnsupportedRefactor;
    const line_start = lineStartOffset(source, param_pos);
    const line_end = lineEndOffset(source, param_pos);
    if (param_pos >= line_end) return error.UnsupportedRefactor;

    const param_open = findByteBack(source, param_pos, line_start, '(') orelse return error.UnsupportedRefactor;
    const param_close = matchingDelimiter(source, param_open, '(', ')', line_end) orelse return error.UnsupportedRefactor;
    const brace = std.mem.indexOfScalarPos(u8, source, param_close, '{') orelse return error.UnsupportedRefactor;
    if (brace >= line_end) return error.UnsupportedRefactor;
    if (std.mem.trim(u8, source[brace + 1 .. line_end], " \t").len != 0) return error.UnsupportedRefactor;

    const line_text = source[line_start..line_end];
    const indent = line_text[0..leadingSpaces(line_text)];

    var params: std.ArrayListUnmanaged(CanonicalParam) = .empty;
    defer {
        for (params.items) |*param| param.deinit(allocator);
        params.deinit(allocator);
    }
    try collectCanonicalParams(allocator, source, param_open + 1, param_close, &params);

    var default_count: usize = 0;
    for (params.items) |param| {
        if (param.default_expr != null) default_count += 1;
    }
    if (default_count == 0) return error.UnsupportedRefactor;

    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);
    try out.appendSlice(allocator, source[line_start .. param_open + 1]);
    for (params.items, 0..) |param, idx| {
        if (idx > 0) try out.appendSlice(allocator, ", ");
        try out.appendSlice(allocator, param.canonical);
    }
    try out.appendSlice(allocator, source[param_close .. brace + 1]);
    for (params.items) |param| {
        const default_expr = param.default_expr orelse continue;
        try out.append(allocator, '\n');
        try out.appendSlice(allocator, indent);
        try out.appendSlice(allocator, "  if (");
        try out.appendSlice(allocator, param.name);
        try out.appendSlice(allocator, " === undefined) { ");
        try out.appendSlice(allocator, param.name);
        try out.appendSlice(allocator, " = ");
        try out.appendSlice(allocator, default_expr);
        try out.appendSlice(allocator, "; }");
    }

    const replacement = try out.toOwnedSlice(allocator);
    errdefer allocator.free(replacement);
    const original = try allocator.dupe(u8, source[line_start..line_end]);
    errdefer allocator.free(original);

    return .{
        .intent = .lift_default_to_body,
        .start_offset = line_start,
        .end_offset = line_end,
        .replacement = replacement,
        .original = original,
    };
}

const CanonicalParam = struct {
    name: []u8,
    canonical: []u8,
    default_expr: ?[]u8 = null,

    fn deinit(self: *CanonicalParam, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.canonical);
        if (self.default_expr) |expr| allocator.free(expr);
        self.* = undefined;
    }
};

fn collectCanonicalParams(
    allocator: std.mem.Allocator,
    source: []const u8,
    params_start: usize,
    params_end: usize,
    out: *std.ArrayListUnmanaged(CanonicalParam),
) !void {
    var start = params_start;
    while (start <= params_end) {
        const end = nextParamEnd(source, start, params_end) orelse return error.UnsupportedRefactor;
        const raw = std.mem.trim(u8, source[start..end], " \t");
        if (raw.len == 0) return error.UnsupportedRefactor;
        var param = try canonicalParamFromText(allocator, raw);
        errdefer param.deinit(allocator);
        try out.append(allocator, param);
        if (end == params_end) break;
        start = end + 1;
    }
}

/// True when `type_text` already has a top-level `undefined` union member, so
/// the canonical optional form needs no extra `| undefined`. Splits on `|` at
/// bracket/angle depth 0 only and matches the member exactly, so an identifier
/// that merely contains the run `undefined` (e.g. `undefinedish`) and a nested
/// `Array<T | undefined>` (an array of optionals, not itself optional) are
/// correctly treated as not-yet-optional.
fn typeIsAlreadyOptional(type_text: []const u8) bool {
    var depth: i32 = 0;
    var seg_start: usize = 0;
    var i: usize = 0;
    while (i <= type_text.len) : (i += 1) {
        if (i == type_text.len or (type_text[i] == '|' and depth == 0)) {
            const seg = std.mem.trim(u8, type_text[seg_start..i], " \t");
            if (std.mem.eql(u8, seg, "undefined")) return true;
            seg_start = i + 1;
            continue;
        }
        switch (type_text[i]) {
            '<', '(', '[', '{' => depth += 1,
            '>', ')', ']', '}' => if (depth > 0) {
                depth -= 1;
            },
            else => {},
        }
    }
    return false;
}

fn canonicalParamFromText(allocator: std.mem.Allocator, raw: []const u8) !CanonicalParam {
    const eq = findTopLevelChar(raw, 0, raw.len, '=');
    const head = std.mem.trim(u8, if (eq) |idx| raw[0..idx] else raw, " \t");
    if (head.len == 0) return error.UnsupportedRefactor;

    const name_end = scanIdentEnd(head, 0, head.len) orelse return error.UnsupportedRefactor;
    const name = head[0..name_end];
    if (!isSimpleIdentifier(name)) return error.UnsupportedRefactor;
    const after_name = std.mem.trim(u8, head[name_end..], " \t");

    var canonical: []u8 = undefined;
    if (after_name.len == 0) {
        canonical = try allocator.dupe(u8, name);
    } else {
        if (after_name[0] != ':') return error.UnsupportedRefactor;
        const type_text = std.mem.trim(u8, after_name[1..], " \t");
        if (type_text.len == 0) return error.UnsupportedRefactor;
        if (typeIsAlreadyOptional(type_text)) {
            canonical = try std.fmt.allocPrint(allocator, "{s}: {s}", .{ name, type_text });
        } else {
            canonical = try std.fmt.allocPrint(allocator, "{s}: {s} | undefined", .{ name, type_text });
        }
    }
    errdefer allocator.free(canonical);

    const default_expr = if (eq) |idx| blk: {
        const expr = std.mem.trim(u8, raw[idx + 1 ..], " \t");
        if (expr.len == 0) return error.UnsupportedRefactor;
        break :blk try allocator.dupe(u8, expr);
    } else null;
    errdefer if (default_expr) |expr| allocator.free(expr);

    return .{
        .name = try allocator.dupe(u8, name),
        .canonical = canonical,
        .default_expr = default_expr,
    };
}

fn nextParamEnd(source: []const u8, start: usize, end: usize) ?usize {
    var i = start;
    var depth: i32 = 0;
    while (i < end) {
        const c = source[i];
        switch (c) {
            '(', '[', '{' => {
                depth += 1;
                i += 1;
            },
            ')', ']', '}' => {
                if (depth == 0) return null;
                depth -= 1;
                i += 1;
            },
            ',' => {
                if (depth == 0) return i;
                i += 1;
            },
            '"', '\'', '`' => {
                const after = scanStringForward(source, i, c) orelse return null;
                if (after > end) return null;
                i = after;
            },
            else => i += 1,
        }
    }
    return end;
}

/// True when `text` has balanced `()`, `[]`, and `{}` delimiters and no
/// unterminated string/template literal. Used to detect an expression that does
/// not finish on its own line, so a single-line rewrite refuses it instead of
/// silently truncating. Strings are skipped with the same `scanStringForward`
/// idiom `nextParamEnd` uses, so a delimiter inside a literal does not count.
fn delimitersBalanced(text: []const u8) bool {
    var depth: i32 = 0;
    var i: usize = 0;
    while (i < text.len) {
        const c = text[i];
        switch (c) {
            '(', '[', '{' => {
                depth += 1;
                i += 1;
            },
            ')', ']', '}' => {
                depth -= 1;
                if (depth < 0) return false;
                i += 1;
            },
            '"', '\'', '`' => {
                i = scanStringForward(text, i, c) orelse return false;
            },
            else => i += 1,
        }
    }
    return depth == 0;
}

/// ZTS618 canonical_destructure_depth: flatten a simple nested object pattern.
///
/// Supported shape:
///   `const {outer: {inner}} = expr;`
///
/// The output names the intermediate object, then destructures the nested
/// fields from that name:
///   `const {outer} = expr;`
///   `const {inner} = outer;`
fn nestedDestructureRewrite(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
    column: u16,
) !StatementRewrite {
    const pos = lineColToOffset(source, line, column) orelse return error.UnsupportedRefactor;
    const line_start = lineStartOffset(source, pos);
    const line_end = lineEndOffset(source, pos);
    const line_text = source[line_start..line_end];
    const trimmed_left = trimLeft(line_text, " \t");
    const indent = line_text[0 .. line_text.len - trimmed_left.len];
    if (!std.mem.startsWith(u8, trimmed_left, "const ")) return error.UnsupportedRefactor;

    const open = std.mem.indexOfScalar(u8, line_text, '{') orelse return error.UnsupportedRefactor;
    const close_abs = matchingDelimiter(source, line_start + open, '{', '}', line_end) orelse return error.UnsupportedRefactor;
    const close = close_abs - line_start;
    const eq = findTopLevelChar(source, close_abs + 1, line_end, '=') orelse return error.UnsupportedRefactor;
    const rhs = std.mem.trim(u8, source[eq + 1 .. line_end], " \t");
    if (rhs.len == 0) return error.UnsupportedRefactor;
    // The replacement captures the RHS only up to this line's newline. If the
    // expression does not finish on this line (an unbalanced `(`/`[`/`{` or an
    // unterminated string, e.g. `= makeUser(\n ...\n)`), refuse rather than emit
    // a truncated, broken two-line replacement. Mirrors the multiline guard the
    // ZTS617 lift rewriter applies to its signature.
    if (!delimitersBalanced(rhs)) return error.UnsupportedRefactor;

    const pattern = std.mem.trim(u8, line_text[open + 1 .. close], " \t");
    const colon = topLevelColon(pattern) orelse return error.UnsupportedRefactor;
    const outer = std.mem.trim(u8, pattern[0..colon], " \t");
    if (!isSimpleIdentifier(outer)) return error.UnsupportedRefactor;
    if (bindingDeclaredBefore(source, line_start, outer)) return error.UnsupportedRefactor;

    const nested = std.mem.trim(u8, pattern[colon + 1 ..], " \t");
    if (nested.len < 2 or nested[0] != '{' or nested[nested.len - 1] != '}') return error.UnsupportedRefactor;
    const inner = std.mem.trim(u8, nested[1 .. nested.len - 1], " \t");
    if (!isFlatIdentifierList(inner)) return error.UnsupportedRefactor;

    const replacement = try std.fmt.allocPrint(
        allocator,
        "{s}const {{{s}}} = {s}\n{s}const {{{s}}} = {s};",
        .{ indent, outer, rhs, indent, inner, outer },
    );
    errdefer allocator.free(replacement);
    const original = try allocator.dupe(u8, source[line_start..line_end]);
    errdefer allocator.free(original);

    return .{
        .intent = .flatten_destructure,
        .start_offset = line_start,
        .end_offset = line_end,
        .replacement = replacement,
        .original = original,
    };
}

/// ZTS619 canonical_unused_index_alias: collapse
/// `for (const pair of items.entries()) { const [_i, item] = pair; ... }`
/// to `for (const item of items) { ... }`.
fn unusedIndexAliasRewrite(
    allocator: std.mem.Allocator,
    source: []const u8,
    line: u32,
    column: u16,
) !StatementRewrite {
    const pos = lineColToOffset(source, line, column) orelse return error.UnsupportedRefactor;
    const for_start = lineStartOffset(source, pos);
    const for_end = lineEndOffset(source, pos);
    const for_line = source[for_start..for_end];
    const for_shape = parseForEntriesLine(for_line) orelse return error.UnsupportedRefactor;

    if (for_end >= source.len or source[for_end] != '\n') return error.UnsupportedRefactor;
    const destructure_start = for_end + 1;
    const destructure_end = lineEndOffset(source, destructure_start);
    const destructure_line = source[destructure_start..destructure_end];
    const destructure = parseIndexAliasDestructure(destructure_line, for_shape.binding) orelse return error.UnsupportedRefactor;

    const span_end = if (destructure_end < source.len and source[destructure_end] == '\n')
        destructure_end + 1
    else
        destructure_end;
    const loop_close = matchingDelimiter(source, for_start + (std.mem.indexOfScalar(u8, for_line, '{') orelse return error.UnsupportedRefactor), '{', '}', source.len) orelse return error.UnsupportedRefactor;
    if (identifierTokenAppears(source[span_end..loop_close], for_shape.binding)) return error.UnsupportedRefactor;

    const replacement = try std.fmt.allocPrint(
        allocator,
        "{s}for (const {s} of {s}) {{\n",
        .{ for_shape.indent, destructure.value, for_shape.iterable },
    );
    errdefer allocator.free(replacement);
    const original = try allocator.dupe(u8, source[for_start..span_end]);
    errdefer allocator.free(original);

    return .{
        .intent = .drop_unused_index_alias,
        .start_offset = for_start,
        .end_offset = span_end,
        .replacement = replacement,
        .original = original,
    };
}

fn lineEndOffset(source: []const u8, pos: usize) usize {
    return std.mem.indexOfScalarPos(u8, source, pos, '\n') orelse source.len;
}

fn findByteBack(source: []const u8, pos: usize, min: usize, target: u8) ?usize {
    var i = pos;
    while (i > min) {
        i -= 1;
        if (source[i] == target) return i;
    }
    return null;
}

fn scanIdentEnd(source: []const u8, start: usize, end: usize) ?usize {
    if (start >= end or !isIdentStart(source[start])) return null;
    var i = start + 1;
    while (i < end and isIdentContinue(source[i])) i += 1;
    return i;
}

fn isSimpleIdentifier(s: []const u8) bool {
    if (s.len == 0 or !isIdentStart(s[0])) return false;
    for (s[1..]) |c| {
        if (!isIdentContinue(c)) return false;
    }
    return true;
}

fn identifierTokenAppears(source: []const u8, ident: []const u8) bool {
    if (ident.len == 0) return false;
    var i: usize = 0;
    while (i < source.len) {
        if (!isIdentStart(source[i])) {
            i += 1;
            continue;
        }
        const start = i;
        i += 1;
        while (i < source.len and isIdentContinue(source[i])) i += 1;
        if (std.mem.eql(u8, source[start..i], ident)) return true;
    }
    return false;
}

fn bindingDeclaredBefore(source: []const u8, stop: usize, ident: []const u8) bool {
    var line_start: usize = 0;
    while (line_start < stop) {
        const raw_end = std.mem.indexOfScalarPos(u8, source, line_start, '\n') orelse stop;
        const line_end = @min(raw_end, stop);
        if (lineDeclaresIdentifier(source[line_start..line_end], ident)) return true;
        if (raw_end >= stop) break;
        line_start = raw_end + 1;
    }
    return false;
}

fn lineDeclaresIdentifier(line: []const u8, ident: []const u8) bool {
    var trimmed = trimLeft(line, " \t");
    if (std.mem.startsWith(u8, trimmed, "export ")) trimmed = trimLeft(trimmed["export ".len..], " \t");

    inline for (.{ "const ", "let ", "var " }) |kw| {
        if (std.mem.startsWith(u8, trimmed, kw)) {
            const decl_start = kw.len;
            const eq = findTopLevelChar(trimmed, decl_start, trimmed.len, '=') orelse trimmed.len;
            return identifierTokenAppears(trimmed[decl_start..eq], ident);
        }
    }

    if (std.mem.startsWith(u8, trimmed, "function ")) {
        const rest = trimmed["function ".len..];
        const name_end = scanIdentEnd(rest, 0, rest.len);
        if (name_end) |end| {
            if (std.mem.eql(u8, rest[0..end], ident)) return true;
        }
        const open = std.mem.indexOfScalar(u8, rest, '(') orelse return false;
        const close = std.mem.indexOfScalarPos(u8, rest, open + 1, ')') orelse return false;
        return identifierTokenAppears(rest[open + 1 .. close], ident);
    }

    return false;
}

fn findTopLevelChar(source: []const u8, start: usize, end: usize, target: u8) ?usize {
    var i = start;
    var depth: i32 = 0;
    while (i < end) {
        const c = source[i];
        switch (c) {
            '(', '[', '{' => {
                depth += 1;
                i += 1;
            },
            ')', ']', '}' => {
                if (depth == 0) return null;
                depth -= 1;
                i += 1;
            },
            '"', '\'', '`' => {
                const after = scanStringForward(source, i, c) orelse return null;
                if (after > end) return null;
                i = after;
            },
            else => {
                if (depth == 0 and c == target) return i;
                i += 1;
            },
        }
    }
    return null;
}

fn matchingDelimiter(source: []const u8, open: usize, open_ch: u8, close_ch: u8, limit: usize) ?usize {
    if (open >= limit or source[open] != open_ch) return null;
    var i = open + 1;
    var depth: u32 = 0;
    while (i < limit) {
        const c = source[i];
        if (c == open_ch) {
            depth += 1;
            i += 1;
            continue;
        }
        if (c == close_ch) {
            if (depth == 0) return i;
            depth -= 1;
            i += 1;
            continue;
        }
        if (c == '"' or c == '\'' or c == '`') {
            const after = scanStringForward(source, i, c) orelse return null;
            if (after > limit) return null;
            i = after;
            continue;
        }
        i += 1;
    }
    return null;
}

fn topLevelColon(source: []const u8) ?usize {
    var i: usize = 0;
    var depth: i32 = 0;
    while (i < source.len) {
        const c = source[i];
        switch (c) {
            '{', '[', '(' => {
                depth += 1;
                i += 1;
            },
            '}', ']', ')' => {
                if (depth == 0) return null;
                depth -= 1;
                i += 1;
            },
            ':' => {
                if (depth == 0) return i;
                i += 1;
            },
            else => i += 1,
        }
    }
    return null;
}

fn isFlatIdentifierList(source: []const u8) bool {
    var rest = std.mem.trim(u8, source, " \t");
    if (rest.len == 0) return false;
    while (true) {
        const comma = std.mem.indexOfScalar(u8, rest, ',');
        const item = std.mem.trim(u8, if (comma) |c| rest[0..c] else rest, " \t");
        if (!isSimpleIdentifier(item)) return false;
        if (comma == null) return true;
        rest = std.mem.trim(u8, rest[comma.? + 1 ..], " \t");
        if (rest.len == 0) return false;
    }
}

const ForEntriesLine = struct {
    indent: []const u8,
    binding: []const u8,
    iterable: []const u8,
};

fn parseForEntriesLine(line: []const u8) ?ForEntriesLine {
    const trimmed = trimLeft(line, " \t");
    const indent = line[0 .. line.len - trimmed.len];
    const prefix = "for (const ";
    if (!std.mem.startsWith(u8, trimmed, prefix)) return null;
    var rest = trimmed[prefix.len..];
    const binding_end = scanIdentEnd(rest, 0, rest.len) orelse return null;
    const binding = rest[0..binding_end];
    if (!isSimpleIdentifier(binding)) return null;
    rest = rest[binding_end..];
    rest = trimLeft(rest, " \t");
    if (!std.mem.startsWith(u8, rest, "of ")) return null;
    rest = trimLeft(rest["of ".len..], " \t");

    const entries = std.mem.lastIndexOf(u8, rest, ".entries()") orelse return null;
    const iterable = std.mem.trim(u8, rest[0..entries], " \t");
    if (iterable.len == 0) return null;
    var tail = trimLeft(rest[entries + ".entries()".len ..], " \t");
    if (tail.len == 0 or tail[0] != ')') return null;
    tail = trimLeft(tail[1..], " \t");
    if (tail.len == 0 or tail[0] != '{') return null;
    if (std.mem.trim(u8, tail[1..], " \t").len != 0) return null;
    return .{ .indent = indent, .binding = binding, .iterable = iterable };
}

const IndexAliasDestructure = struct {
    value: []const u8,
};

fn parseIndexAliasDestructure(line: []const u8, pair_binding: []const u8) ?IndexAliasDestructure {
    const trimmed = trimLeft(line, " \t");
    const prefix = "const [";
    if (!std.mem.startsWith(u8, trimmed, prefix)) return null;
    const close = std.mem.indexOfScalar(u8, trimmed, ']') orelse return null;
    const pattern = trimmed[prefix.len..close];
    const comma = std.mem.indexOfScalar(u8, pattern, ',') orelse return null;
    if (std.mem.indexOfScalarPos(u8, pattern, comma + 1, ',') != null) return null;
    const index_name = std.mem.trim(u8, pattern[0..comma], " \t");
    const value_name = std.mem.trim(u8, pattern[comma + 1 ..], " \t");
    if (!isSimpleIdentifier(index_name) or !isSimpleIdentifier(value_name)) return null;

    var tail = trimLeft(trimmed[close + 1 ..], " \t");
    if (tail.len == 0 or tail[0] != '=') return null;
    tail = std.mem.trim(u8, tail[1..], " \t;");
    if (!std.mem.eql(u8, tail, pair_binding)) return null;
    return .{ .value = value_name };
}

fn appendStatementRewriteUnique(
    allocator: std.mem.Allocator,
    result: *StatementRewriteResult,
    rewrite: StatementRewrite,
) !void {
    var owned = rewrite;
    for (result.rewrites.items) |existing| {
        if (existing.start_offset == owned.start_offset and existing.end_offset == owned.end_offset) {
            owned.deinit(allocator);
            return;
        }
    }
    result.rewrites.append(allocator, owned) catch |err| {
        owned.deinit(allocator);
        return err;
    };
}

/// Select the subset of `rewrites` that can be applied together in one pass:
/// drop any rewrite whose span is strictly contained in another (apply the
/// inner one first, post-order), and any whose span overlaps another without
/// nesting (ambiguous; leave for a later pass). The fixed-point loop re-derives
/// rewrites against the new source each pass, so a dropped outer rewrite is
/// retried once its inner children are resolved. The returned slice borrows
/// from `rewrites`; the caller owns the backing list.
fn selectNonOverlapping(
    allocator: std.mem.Allocator,
    rewrites: []const StatementRewrite,
) ![]const StatementRewrite {
    var keep: std.ArrayListUnmanaged(StatementRewrite) = .empty;
    errdefer keep.deinit(allocator);
    outer: for (rewrites, 0..) |a, i| {
        for (rewrites, 0..) |b, j| {
            if (i == j) continue;
            const a_contains_b = a.start_offset <= b.start_offset and b.end_offset <= a.end_offset;
            const b_contains_a = b.start_offset <= a.start_offset and a.end_offset <= b.end_offset;
            const disjoint = a.end_offset <= b.start_offset or b.end_offset <= a.start_offset;
            // `a` is dropped this pass when it strictly contains another span:
            // apply the inner one first (post-order), retry `a` next pass once
            // its child is resolved.
            if (a_contains_b and !b_contains_a) continue :outer;
            // Partial overlap with neither containing the other is ambiguous;
            // keep only the earlier-starting span this pass so the result is
            // deterministic. (Equal spans are de-duplicated before this point.)
            if (!disjoint and !a_contains_b and !b_contains_a and a.start_offset > b.start_offset) continue :outer;
        }
        try keep.append(allocator, a);
    }
    return keep.toOwnedSlice(allocator);
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

// ---------------------------------------------------------------------------
// Fixed-point normalizer
// ---------------------------------------------------------------------------

/// Backstop on the rewrite loop. Each pass strictly reduces the count of
/// canonical-band diagnostics, so a real handler converges in a handful of
/// passes; the cap only defends against a future refactor that fails to reduce
/// the measure (which must never silently report `canonical`). Hitting the cap
/// leaves `converged = false`.
pub const max_normalize_iterations: u32 = 64;

fn isCanonicalBandCode(code: []const u8) bool {
    return zigts.rule_registry.isCanonicalProfileCode(code);
}

/// Map a `Refactor.kind` string to the typed `RepairIntent` it realizes, so a
/// normalize pass records *what* it rewrote without re-parsing prose. Returns
/// null for kinds with no 1:1 intent (none today; future-proofing).
fn repairIntentForKind(kind: []const u8) ?RepairIntent {
    if (std.mem.eql(u8, kind, "canonicalize_arrow_helper")) return .replace_arrow_with_function;
    if (std.mem.eql(u8, kind, "canonicalize_export_function")) return .replace_export_arrow_with_function;
    if (std.mem.eql(u8, kind, "canonicalize_let_const")) return .replace_let_with_const;
    if (std.mem.eql(u8, kind, "canonicalize_for_of_const")) return .canonicalize_for_of_const;
    if (std.mem.eql(u8, kind, "canonicalize_compound_assign")) return .replace_compound_assign_with_explicit;
    if (std.mem.eql(u8, kind, "canonicalize_capability_key_alias")) return .canonicalize_capability_key_alias;
    if (std.mem.eql(u8, kind, "canonicalize_redundant_bool_compare")) return .drop_redundant_bool_compare;
    return null;
}

pub const NormalizeResult = struct {
    /// The handler source after reaching the rewrite fixed point. Owned.
    canonical_source: []u8,
    /// Ordered intents applied across all passes (one per rewritten node).
    rewrite_trace: std.ArrayListUnmanaged(RepairIntent) = .empty,
    /// True when the loop reached a fixed point (no refactor fired) rather than
    /// hitting the iteration cap or aborting on a per-pass gate.
    converged: bool,
    /// True iff zero canonical-band diagnostics remain in `canonical_source`.
    /// Distinct from `converged`: a handler with an unrewritten ternary
    /// converges (no refactor fires) yet is not fully canonical.
    fully_canonical: bool,
    /// Passes actually run.
    iterations: u32,
    /// Canonical-band diagnostics still present after normalization (0 when
    /// `fully_canonical`).
    residual: u32,
    /// Owned details for the remaining canonical-profile diagnostics. Empty
    /// when `fully_canonical`.
    residual_diagnostics: std.ArrayListUnmanaged(ResidualDiagnostic) = .empty,

    pub fn deinit(self: *NormalizeResult, allocator: std.mem.Allocator) void {
        allocator.free(self.canonical_source);
        self.rewrite_trace.deinit(allocator);
        for (self.residual_diagnostics.items) |*diag| diag.deinit(allocator);
        self.residual_diagnostics.deinit(allocator);
        self.* = undefined;
    }
};

pub const ResidualDiagnostic = struct {
    code: []u8,
    severity: []u8,
    message: []u8,
    file: []u8,
    line: u32,
    column: u16,
    suggestion: ?[]u8,
    repair_intent: ?[]const u8,
    reason: []const u8,

    pub fn deinit(self: *ResidualDiagnostic, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
        allocator.free(self.severity);
        allocator.free(self.message);
        allocator.free(self.file);
        if (self.suggestion) |s| allocator.free(s);
        self.* = undefined;
    }
};

/// Normalize a handler file: read it, then drive `normalizeSource`.
pub fn normalize(allocator: std.mem.Allocator, file: []const u8) !NormalizeResult {
    const source = try zigts.file_io.readFile(allocator, file, 10 * 1024 * 1024);
    defer allocator.free(source);
    return normalizeSource(allocator, source, file);
}

/// Reduce `source` to its Canonical Normal Form by applying every available
/// canonical refactor to a fixed point. Confluent and terminating: at most one
/// refactor applies per source line per pass (`applyRefactors` rejects
/// overlaps), every emitted refactor is a closed-form local rewrite whose
/// output carries no refactorable node of the same kind, and each pass strictly
/// reduces the canonical-diagnostic measure. A per-pass `edit_simulate` gate
/// rejects any pass that would introduce a new violation.
pub fn normalizeSource(
    allocator: std.mem.Allocator,
    source: []const u8,
    virtual_path: []const u8,
) !NormalizeResult {
    var current = try allocator.dupe(u8, source);
    errdefer allocator.free(current);
    var trace: std.ArrayListUnmanaged(RepairIntent) = .empty;
    errdefer trace.deinit(allocator);

    var iterations: u32 = 0;
    var converged = false;
    while (iterations < max_normalize_iterations) {
        var check = try precompile.runCheckOnlyFromSource(allocator, current, virtual_path, null, true, null, false);
        defer check.deinit(allocator);
        const cur_band = countBand(check.json_diagnostics.items);

        // Line-keyed single-line refactors take priority each pass; they are
        // the original Phase-1 rewriters and rely on `applyRefactors`' overlap
        // guard.
        var result = Result{ .file = virtual_path };
        defer result.deinit(allocator);
        try buildRefactors(allocator, current, check.json_diagnostics.items, &result);

        // Span-keyed multi-line / convention rewrites for this pass.
        var stmt_result = StatementRewriteResult{};
        defer stmt_result.deinit(allocator);
        try buildStatementRewrites(allocator, current, check.json_diagnostics.items, &stmt_result);

        if (result.refactors.items.len == 0 and stmt_result.rewrites.items.len == 0) {
            converged = true;
            break;
        }

        const Step = struct { next: []u8, intents: []const RepairIntent, intents_owned: bool };
        const step: ?Step = blk: {
            if (result.refactors.items.len > 0) {
                const next = applyRefactors(allocator, current, result.refactors.items) catch |err| switch (err) {
                    // A pass we cannot apply deterministically (two refactors on
                    // one line, an unexpected multi-line replacement, a stale
                    // line) stops the loop short of a fixed point rather than
                    // guessing.
                    error.OverlappingRefactors,
                    error.UnsupportedRefactor,
                    error.StaleRefactorLine,
                    error.RefactorLineNotFound,
                    => break :blk null,
                    else => return err,
                };
                break :blk .{ .next = next, .intents = &.{}, .intents_owned = false };
            }
            // No single-line refactors this pass: apply the innermost
            // non-overlapping subset of span rewrites, post-order.
            const subset = selectNonOverlapping(allocator, stmt_result.rewrites.items) catch |err| return err;
            defer allocator.free(subset);
            if (subset.len == 0) break :blk null;
            const next = applyStatementRewrites(allocator, current, subset) catch |err| switch (err) {
                error.OverlappingRefactors,
                error.StaleRefactorLine,
                error.RefactorLineNotFound,
                => break :blk null,
                else => return err,
            };
            const intents = try allocator.alloc(RepairIntent, subset.len);
            for (subset, 0..) |rw, i| intents[i] = rw.intent;
            break :blk .{ .next = next, .intents = intents, .intents_owned = true };
        };
        const s = step orelse break;
        const next = s.next;

        // Per-pass gate. Reject a rewrite that fails to parse/type-check (a
        // malformed replacement) or that does not strictly reduce the
        // canonical-band measure (no progress -> would loop to the cap).
        // Spec-discharge / flow diagnostics that a *fixed* strict error
        // unmasks are deliberately ignored: they are latent properties of the
        // handler, not introduced by the rewrite, and removing the masking
        // strict error is exactly the canonicalization we want.
        var next_check = try precompile.runCheckOnlyFromSource(allocator, next, virtual_path, null, true, null, false);
        const hard_errors = next_check.parse_errors + next_check.type_errors + next_check.bool_errors;
        const next_band = countBand(next_check.json_diagnostics.items);
        next_check.deinit(allocator);
        if (hard_errors > 0 or next_band >= cur_band) {
            allocator.free(next);
            if (s.intents_owned) allocator.free(s.intents);
            break;
        }

        // Record the applied intents. On an OOM here, free the freshly-built
        // `next` and any owned `intents` before propagating so the only live
        // allocations remain `current` and `trace` (both covered by errdefer).
        {
            errdefer allocator.free(next);
            errdefer if (s.intents_owned) allocator.free(s.intents);
            if (result.refactors.items.len > 0) {
                for (result.refactors.items) |r| {
                    if (repairIntentForKind(r.kind)) |intent| try trace.append(allocator, intent);
                }
            } else {
                for (s.intents) |intent| try trace.append(allocator, intent);
            }
        }
        if (s.intents_owned) allocator.free(s.intents);
        allocator.free(current);
        current = next;
        iterations += 1;
    }

    var residual_diagnostics = try collectCanonicalResidualDiagnostics(allocator, current, virtual_path);
    errdefer {
        for (residual_diagnostics.items) |*diag| diag.deinit(allocator);
        residual_diagnostics.deinit(allocator);
    }
    const residual: u32 = @intCast(residual_diagnostics.items.len);
    return .{
        .canonical_source = current,
        .rewrite_trace = trace,
        .converged = converged,
        .fully_canonical = residual == 0,
        .iterations = iterations,
        .residual = residual,
        .residual_diagnostics = residual_diagnostics,
    };
}

/// Count the canonical-band diagnostics in a diagnostic set.
fn countBand(diagnostics: []const precompile.json_diag.JsonDiagnostic) u32 {
    var n: u32 = 0;
    for (diagnostics) |diag| {
        if (isCanonicalBandCode(diag.code)) n += 1;
    }
    return n;
}

/// Collect canonical-band diagnostics remaining in `source`.
fn collectCanonicalResidualDiagnostics(
    allocator: std.mem.Allocator,
    source: []const u8,
    virtual_path: []const u8,
) !std.ArrayListUnmanaged(ResidualDiagnostic) {
    var check = try precompile.runCheckOnlyFromSource(allocator, source, virtual_path, null, true, null, false);
    defer check.deinit(allocator);

    var out: std.ArrayListUnmanaged(ResidualDiagnostic) = .empty;
    errdefer {
        for (out.items) |*diag| diag.deinit(allocator);
        out.deinit(allocator);
    }
    for (check.json_diagnostics.items) |diag| {
        if (!isCanonicalBandCode(diag.code)) continue;
        var owned = try cloneResidualDiagnostic(allocator, diag);
        errdefer owned.deinit(allocator);
        try out.append(allocator, owned);
    }
    return out;
}

fn cloneResidualDiagnostic(
    allocator: std.mem.Allocator,
    diag: precompile.json_diag.JsonDiagnostic,
) !ResidualDiagnostic {
    const rule = zigts.rule_registry.findByCode(diag.code);
    var out = ResidualDiagnostic{
        .code = try allocator.dupe(u8, diag.code),
        .severity = &.{},
        .message = &.{},
        .file = &.{},
        .line = diag.line,
        .column = diag.column,
        .suggestion = null,
        .repair_intent = if (rule) |r| if (r.repair) |intent| intent.asString() else null else null,
        .reason = "no behavior-preserving normalizer rewrite applied",
    };
    errdefer out.deinit(allocator);
    out.severity = try allocator.dupe(u8, diag.severity);
    out.message = try allocator.dupe(u8, diag.message);
    out.file = try allocator.dupe(u8, diag.file);
    if (diag.suggestion) |s| out.suggestion = try allocator.dupe(u8, s);
    return out;
}

pub fn writeNormalizeJson(
    writer: anytype,
    file: []const u8,
    nr: *const NormalizeResult,
    written: bool,
) !void {
    const hash = zigts.rule_registry.policyHash();
    try writer.writeAll("{\"ok\":true,\"file\":");
    try writeJsonString(writer, file);
    try writer.writeAll(",\"policy_hash\":");
    try writeJsonString(writer, &hash);
    try writer.print(
        ",\"converged\":{},\"fullyCanonical\":{},\"iterations\":{d},\"residual\":{d},\"written\":{}",
        .{ nr.converged, nr.fully_canonical, nr.iterations, nr.residual, written },
    );
    try writer.writeAll(",\"rewriteTrace\":[");
    for (nr.rewrite_trace.items, 0..) |intent, i| {
        if (i > 0) try writer.writeByte(',');
        try writeJsonString(writer, intent.asString());
    }
    try writer.writeAll("],\"residualDiagnostics\":[");
    for (nr.residual_diagnostics.items, 0..) |*diag, i| {
        if (i > 0) try writer.writeByte(',');
        try writeResidualDiagnosticJson(writer, diag);
    }
    try writer.writeAll("],\"canonicalSource\":");
    try writeJsonString(writer, nr.canonical_source);
    try writer.writeAll("}\n");
}

fn writeResidualDiagnosticJson(writer: anytype, diag: *const ResidualDiagnostic) !void {
    try writer.writeAll("{\"code\":");
    try writeJsonString(writer, diag.code);
    try writer.writeAll(",\"severity\":");
    try writeJsonString(writer, diag.severity);
    try writer.writeAll(",\"message\":");
    try writeJsonString(writer, diag.message);
    try writer.writeAll(",\"file\":");
    try writeJsonString(writer, diag.file);
    try writer.print(",\"line\":{d},\"column\":{d}", .{ diag.line, diag.column });
    try writer.writeAll(",\"suggestion\":");
    if (diag.suggestion) |s| {
        try writeJsonString(writer, s);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\"repairIntent\":");
    if (diag.repair_intent) |intent| {
        try writeJsonString(writer, intent);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(",\"reason\":");
    try writeJsonString(writer, diag.reason);
    try writer.writeByte('}');
}

/// `zigts normalize <file> [--write] [--check] [--json]` — the gofmt-for-
/// semantics surface. Default prints canonical source; `--write` rewrites in
/// place (refusing unless fully canonical); `--check` exits 1 when not yet
/// canonical (CI gate); `--json` emits a structured envelope with the rewrite
/// trace. Reachable as both `zigts normalize` and `zigttp normalize`.
pub fn runNormalizeWithArgs(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var file: ?[]const u8 = null;
    var write_mode = false;
    var check_mode = false;
    var json_mode = false;
    for (argv) |arg| {
        if (std.mem.eql(u8, arg, "--write")) {
            write_mode = true;
        } else if (std.mem.eql(u8, arg, "--check")) {
            check_mode = true;
        } else if (std.mem.eql(u8, arg, "--json")) {
            json_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            printNormalizeHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-") and file == null) {
            file = arg;
        } else {
            return error.InvalidArgument;
        }
    }
    const path = file orelse {
        const usage = "Usage: zigts normalize <file> [--write] [--check] [--json]\n";
        _ = std.c.write(std.c.STDERR_FILENO, usage.ptr, usage.len);
        std.process.exit(1);
    };
    if (write_mode and check_mode) return error.InvalidArgument;

    var nr = try normalize(allocator, path);
    defer nr.deinit(allocator);

    if (json_mode) {
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(allocator);
        var aw: std.Io.Writer.Allocating = .fromArrayList(allocator, &buf);
        try writeNormalizeJson(&aw.writer, path, &nr, write_mode and nr.fully_canonical and nr.converged);
        buf = aw.toArrayList();
        if (buf.items.len > 0) _ = std.c.write(std.c.STDOUT_FILENO, buf.items.ptr, buf.items.len);
    }

    if (check_mode) {
        // gofmt -l semantics: the file is "clean" only if it is ALREADY in
        // canonical form, i.e. normalize changed nothing (iterations == 0) and
        // no canonical-band diagnostic remains. A file that normalizes cleanly
        // but needed rewrites is still reported as not-canonical (exit 1).
        if (nr.iterations > 0 or !nr.fully_canonical) {
            if (!json_mode) {
                _ = std.c.write(std.c.STDERR_FILENO, path.ptr, path.len);
                _ = std.c.write(std.c.STDERR_FILENO, "\n", 1);
            }
            std.process.exit(1);
        }
        return;
    }

    if (write_mode) {
        // Never silently accept a partial normalization: refuse the write when
        // the loop did not reach a fixed point or a canonical-band diagnostic
        // remains (an unrewritten construct from a not-yet-implemented phase).
        if (!nr.converged or !nr.fully_canonical) {
            if (!json_mode) {
                const msg = "normalize: not fully canonical; refusing --write (run `zigts check` for residual)\n";
                _ = std.c.write(std.c.STDERR_FILENO, msg.ptr, msg.len);
            }
            std.process.exit(1);
        }
        try zigts.file_io.writeFile(allocator, path, nr.canonical_source);
        return;
    }

    if (!json_mode and nr.canonical_source.len > 0) {
        _ = std.c.write(std.c.STDOUT_FILENO, nr.canonical_source.ptr, nr.canonical_source.len);
    }
}

fn printNormalizeHelp() void {
    const help =
        \\zigts normalize - rewrite a handler into Canonical Normal Form
        \\
        \\Usage: zigts normalize <file> [--write] [--check] [--json]
        \\
        \\  (default)  print the canonical source to stdout
        \\  --write    rewrite the file in place (refuses unless fully canonical)
        \\  --check    exit 1 if the file is not already canonical (CI gate)
        \\  --json     emit a structured envelope with the rewrite trace
        \\
    ;
    _ = std.c.write(std.c.STDOUT_FILENO, help.ptr, help.len);
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
    const path = file orelse {
        const usage = "Usage: zigts canonicalize <file> --json [--simulate]\n";
        _ = std.c.write(std.c.STDERR_FILENO, usage.ptr, usage.len);
        std.process.exit(1);
    };
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

test "redundantBoolCompareReplacement rewrites `=== true` to bare boolean" {
    // `  const ok = ready === true;` -- `===` operator starts at column 20.
    const got = try redundantBoolCompareReplacement(std.testing.allocator, "  const ok = ready === true;", 20, true);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("  const ok = ready;", got);
}

test "redundantBoolCompareReplacement rewrites `=== false` to negation" {
    const got = try redundantBoolCompareReplacement(std.testing.allocator, "  const ok = ready === false;", 20, false);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("  const ok = !ready;", got);
}

test "redundantBoolCompareReplacement rewrites `!== false` to bare boolean" {
    const got = try redundantBoolCompareReplacement(std.testing.allocator, "  const ok = ready !== false;", 20, true);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("  const ok = ready;", got);
}

test "redundantBoolCompareReplacement rewrites literal-on-left form" {
    // `  if (true === ready) { ... }` -- `===` operator starts at column 12.
    const got = try redundantBoolCompareReplacement(std.testing.allocator, "  if (true === ready) { return Response.text(\"a\"); }", 12, true);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("  if (ready) { return Response.text(\"a\"); }", got);
}

test "redundantBoolCompareReplacement preserves trailing line content for `if`" {
    // `  if (ready === true) { ... }` -- `===` operator starts at column 13.
    const got = try redundantBoolCompareReplacement(std.testing.allocator, "  if (ready === true) { return Response.text(\"a\"); }", 13, true);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("  if (ready) { return Response.text(\"a\"); }", got);
}

test "redundantBoolCompareReplacement keeps member-access value operand" {
    // `  const ok = req.ready === true;` -- `===` operator starts at column 24.
    const got = try redundantBoolCompareReplacement(std.testing.allocator, "  const ok = req.ready === true;", 24, true);
    defer std.testing.allocator.free(got);
    try std.testing.expectEqualStrings("  const ok = req.ready;", got);
}

test "redundantBoolCompareReplacement refuses a non-simple value operand" {
    // A call-valued operand (`ready()`) cannot be re-spelled as a bare boolean
    // without re-deriving its span and risking a behavior change: refuse it.
    // `  const ok = ready() === true;` -- `===` operator starts at column 22.
    try std.testing.expectError(
        error.UnsupportedRefactor,
        redundantBoolCompareReplacement(std.testing.allocator, "  const ok = ready() === true;", 22, true),
    );
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
    const virtual_path = "handler.ts";
    var preview = try collectFromSource(std.testing.allocator, source, virtual_path);
    defer preview.deinit(std.testing.allocator);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);
    try writeJson(&aw.writer, &preview);
    buf = aw.toArrayList();

    const json = try buf.toOwnedSlice(std.testing.allocator);
    errdefer std.testing.allocator.free(json);
    // Dupe so callers can keep their `defer free(out.file)` symmetry.
    const file = try std.testing.allocator.dupe(u8, virtual_path);
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

    // Both lets are rewritten and no canonical diagnostic remains: re-collecting
    // on the multi-line rewrite yields zero refactors. (edit_simulate's
    // new_count would also count the latent ZTS500 this Spec-less handler always
    // had, which the rewrite did not introduce.)
    var after = try collectFromSource(std.testing.allocator, proposed, "handler.ts");
    defer after.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), after.refactors.items.len);
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
    var preview = try collectFromSource(std.testing.allocator, source, "handler.ts");
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

    // Applying the refactor clears the canonical diagnostic: re-collecting on
    // the rewritten source yields no further refactor. (Asserting on the full
    // diagnostic total would be wrong: removing the masking strict error
    // unmasks a latent ZTS500 spec-discharge diagnostic this handler always
    // had, which the rewrite did not introduce.)
    var after = try collectFromSource(std.testing.allocator, proposed, "handler.ts");
    defer after.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), after.refactors.items.len);
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
    var preview = try collectFromSource(std.testing.allocator, source, "handler.ts");
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

    // Re-collecting on the rewritten source yields no further refactor: the
    // capability-alias canonical diagnostic is cleared by the rewrite.
    var after = try collectFromSource(std.testing.allocator, proposed.items, "handler.ts");
    defer after.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), after.refactors.items.len);
}

test "normalizeSource fixes avoidable let to const and is fully canonical" {
    const source =
        \\function handler(req: Request): Response {
        \\  let count = 1;
        \\  return Response.json({ count });
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(nr.iterations >= 1);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "const count = 1;") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "let count") == null);
}

test "normalizeSource records the rewrite trace" {
    const source =
        \\function handler(req: Request): Response {
        \\  let count = 1;
        \\  return Response.json({ count });
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.rewrite_trace.items.len >= 1);
    try std.testing.expectEqual(RepairIntent.replace_let_with_const, nr.rewrite_trace.items[0]);
}

test "normalizeSource rewrites `=== true` comparison and is fully canonical" {
    const source =
        \\function handler(req: Request): Response {
        \\  const ready = req.method === "GET";
        \\  if (ready === true) { return Response.text("ready"); }
        \\  return Response.text("not");
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(nr.iterations >= 1);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "if (ready)") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "=== true") == null);

    var found = false;
    for (nr.rewrite_trace.items) |intent| {
        if (intent == .drop_redundant_bool_compare) found = true;
    }
    try std.testing.expect(found);
}

test "normalizeSource rewrites `=== false` comparison to negation" {
    const source =
        \\function handler(req: Request): Response {
        \\  const ready = req.method === "GET";
        \\  if (ready === false) { return Response.text("not"); }
        \\  return Response.text("ready");
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "if (!ready)") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "=== false") == null);
}

test "redundant-bool-compare rewrite is behavior-equivalent (contract diff)" {
    // The non-canonical `before` carries a ZTS620 hard error, so it never
    // extracts a contract. Drive the rewriter to its canonical output, then
    // prove that output is behaviorally equivalent to an independently
    // hand-written reference handler that expresses the same two response
    // paths with the bare boolean. Both are canonical, so both extract a
    // contract; `diffContracts` compares the observable behavior.
    const before =
        \\function handler(req: Request): Response {
        \\  const ready = req.method === "GET";
        \\  if (ready === true) { return Response.text("ready"); }
        \\  return Response.text("not");
        \\}
    ;
    const reference =
        \\function handler(req: Request): Response {
        \\  const ready = req.method === "GET";
        \\  if (ready) { return Response.text("ready"); }
        \\  return Response.text("not");
        \\}
    ;

    var nr = try normalizeSource(std.testing.allocator, before, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);

    var lhs = try precompile.runCheckOnlyFromSource(std.testing.allocator, nr.canonical_source, "handler.ts", null, true, null, false);
    defer lhs.deinit(std.testing.allocator);
    var rhs = try precompile.runCheckOnlyFromSource(std.testing.allocator, reference, "handler.ts", null, true, null, false);
    defer rhs.deinit(std.testing.allocator);

    const lhs_contract = lhs.contract orelse return error.NoContractFromCanonicalOutput;
    const rhs_contract = rhs.contract orelse return error.NoContractFromReference;

    var diff = try zigts.contract_diff.diffContracts(std.testing.allocator, &lhs_contract, &rhs_contract);
    defer diff.deinit(std.testing.allocator);
    try std.testing.expect(diff.behavioralVerdict().isSafeNoOp());
}

test "normalizeSource: ternary is rewritten to an expression-position match and is fully canonical" {
    // ZTS612 lands as a span-keyed rewrite: `cond ? a : b` becomes
    // `match (!!(cond)) { when true: a, default: b }`, which is valid in the
    // const-initializer position the ternary occupied. The loop converges with
    // no residual canonical-band diagnostic.
    const source =
        \\function handler(req: Request): Response {
        \\  const ok = req.method === "GET";
        \\  const status = ok ? 200 : 500;
        \\  return Response.json({ status });
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(nr.iterations >= 1);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "match (!!(ok)) { when true: 200, default: 500 }") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "?") == null);

    var found = false;
    for (nr.rewrite_trace.items) |intent| {
        if (intent == .replace_ternary_with_if) found = true;
    }
    try std.testing.expect(found);
}

test "normalizeSource: ternary with a relational condition parenthesizes the whole condition" {
    // Regression guard: a bare `!!cond` mis-parses as `(!!a) === b` when the
    // condition ends in a relational/equality operator, silently flipping the
    // branch selection. The condition must be parenthesized: `!!(a === b)`.
    // (Contract-diff equivalence compares surfaces, not expression semantics,
    // so this is asserted textually.)
    const source =
        \\function handler(req: Request): Response {
        \\  const status = req.method === "GET" ? 200 : 500;
        \\  return Response.json({ status });
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "match (!!(req.method === \"GET\"))") != null);
    // The broken precedence form must never appear.
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "!!req.method") == null);
}

test "normalizeSource is idempotent on a reused arrow helper" {
    const source =
        \\const parse = (x: number): number => x;
        \\function handler(req: Request): Response {
        \\  const a = parse(1);
        \\  const b = parse(2);
        \\  return Response.json({ a, b });
        \\}
    ;
    var first = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer first.deinit(std.testing.allocator);
    try std.testing.expect(first.fully_canonical);
    try std.testing.expect(std.mem.indexOf(u8, first.canonical_source, "function parse") != null);

    var second = try normalizeSource(std.testing.allocator, first.canonical_source, "handler.ts");
    defer second.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings(first.canonical_source, second.canonical_source);
    // A normalized handler is a fixed point: re-normalizing applies nothing.
    try std.testing.expectEqual(@as(u32, 0), second.iterations);
}

test "normalizeSource: a ternary in an arrow body bounds the condition at `=>`" {
    // Regression: ternaryConditionStart must treat `=>` as a boundary, not sweep
    // the arrow params into the condition (which would yield an always-truthy
    // `match (!!((x) => cond))`).
    const source =
        \\const clamp = (x: number): number => x > 0 ? 1 : -1;
        \\function handler(req: Request): Response {
        \\  const v = clamp(2);
        \\  return Response.json({ v });
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "match (!!(x > 0))") != null);
    // The broken form would have swept the arrow params into the condition.
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "!!((x") == null);
}

test "scanOperandTokenBack refuses a member tail of a call/index chain" {
    // `obj().foo`: the `.` before `foo` is preceded by `)`, so the operand
    // cannot be bounded line-locally. Refuse rather than return the partial
    // `foo` (which would splice `!foo` after `obj().`, producing `obj().!foo`).
    const line = "  const ok = obj().foo === false;";
    const foo_end = std.mem.indexOf(u8, line, "foo").? + 3;
    try std.testing.expectEqual(@as(?usize, null), scanOperandTokenBack(line, foo_end));
    // A plain dotted chain is still accepted.
    const ok_line = "  const ok = a.b.c === true;";
    const c_end = std.mem.indexOf(u8, ok_line, "c ").? + 1;
    try std.testing.expect(scanOperandTokenBack(ok_line, c_end) != null);
}

test "templateHoistRewrite refuses when a prior statement shares the physical line" {
    // `foo(); const g = ...`: hoisting at line_start would evaluate the
    // interpolation before foo(), reordering side effects.
    const source = "  foo(); const g = `Hi ${u.up()}!`;\n";
    const bt = std.mem.indexOfScalar(u8, source, '`').?;
    try std.testing.expectError(
        error.UnsupportedRefactor,
        templateHoistRewrite(std.testing.allocator, source, 1, @intCast(bt + 1)),
    );
}

// ---------------------------------------------------------------------------
// ZTS612 ternary -> match (span-keyed)
// ---------------------------------------------------------------------------

test "applyStatementRewrites splices a single span and validates the snapshot" {
    const source = "const x = a ? 1 : 2;\n";
    const rw = StatementRewrite{
        .intent = .replace_ternary_with_if,
        .start_offset = 10,
        .end_offset = 19,
        .replacement = try std.testing.allocator.dupe(u8, "match (!!a) { when true: 1, default: 2 }"),
        .original = try std.testing.allocator.dupe(u8, "a ? 1 : 2"),
    };
    var rws = [_]StatementRewrite{rw};
    defer for (&rws) |*r| r.deinit(std.testing.allocator);
    const out = try applyStatementRewrites(std.testing.allocator, source, &rws);
    defer std.testing.allocator.free(out);
    try std.testing.expectEqualStrings("const x = match (!!a) { when true: 1, default: 2 };\n", out);
}

test "applyStatementRewrites rejects a stale snapshot" {
    const source = "const x = a ? 1 : 2;\n";
    var rw = StatementRewrite{
        .intent = .replace_ternary_with_if,
        .start_offset = 10,
        .end_offset = 19,
        .replacement = try std.testing.allocator.dupe(u8, "X"),
        .original = try std.testing.allocator.dupe(u8, "b ? 1 : 2"), // wrong
    };
    defer rw.deinit(std.testing.allocator);
    var rws = [_]StatementRewrite{rw};
    try std.testing.expectError(error.StaleRefactorLine, applyStatementRewrites(std.testing.allocator, source, &rws));
}

test "applyStatementRewrites rejects overlapping spans" {
    const source = "abcdefghij";
    var a = StatementRewrite{
        .intent = .replace_ternary_with_if,
        .start_offset = 0,
        .end_offset = 5,
        .replacement = try std.testing.allocator.dupe(u8, "X"),
        .original = try std.testing.allocator.dupe(u8, "abcde"),
    };
    var b = StatementRewrite{
        .intent = .replace_ternary_with_if,
        .start_offset = 3,
        .end_offset = 8,
        .replacement = try std.testing.allocator.dupe(u8, "Y"),
        .original = try std.testing.allocator.dupe(u8, "defgh"),
    };
    defer a.deinit(std.testing.allocator);
    defer b.deinit(std.testing.allocator);
    var rws = [_]StatementRewrite{ a, b };
    try std.testing.expectError(error.OverlappingRefactors, applyStatementRewrites(std.testing.allocator, source, &rws));
}

test "lineColToOffset maps a 1-based position to a byte offset" {
    const source = "ab\ncde\nfgh";
    try std.testing.expectEqual(@as(?usize, 0), lineColToOffset(source, 1, 1));
    try std.testing.expectEqual(@as(?usize, 4), lineColToOffset(source, 2, 2)); // 'd'
    try std.testing.expectEqual(@as(?usize, 7), lineColToOffset(source, 3, 1)); // 'f'
    try std.testing.expectEqual(@as(?usize, null), lineColToOffset(source, 9, 1));
}

test "ternaryToMatchRewrite builds the canonical match form for a const initializer" {
    const source = "  const status = ok ? 200 : 500;\n";
    // `?` is at 1-based column 21 on line 1.
    var rw = try ternaryToMatchRewrite(std.testing.allocator, source, 1, 21);
    defer rw.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("ok ? 200 : 500", rw.original);
    try std.testing.expectEqualStrings("match (!!(ok)) { when true: 200, default: 500 }", rw.replacement);
    try std.testing.expectEqual(RepairIntent.replace_ternary_with_if, rw.intent);
}

test "ternaryToMatchRewrite handles a return-position ternary" {
    const source = "  return ok ? Response.text(\"y\") : Response.text(\"n\");\n";
    // `?` after `ok` is at column 13.
    var rw = try ternaryToMatchRewrite(std.testing.allocator, source, 1, 13);
    defer rw.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("ok ? Response.text(\"y\") : Response.text(\"n\")", rw.original);
    try std.testing.expectEqualStrings(
        "match (!!(ok)) { when true: Response.text(\"y\"), default: Response.text(\"n\") }",
        rw.replacement,
    );
}

test "ternaryToMatchRewrite refuses an optional chain and a nullish operator" {
    // `?.` is at column 4; `??` is at column 4 in the second source.
    const oc = "  a?.b;\n";
    try std.testing.expectError(error.UnsupportedRefactor, ternaryToMatchRewrite(std.testing.allocator, oc, 1, 4));
    const nc = "  a ?? b;\n";
    try std.testing.expectError(error.UnsupportedRefactor, ternaryToMatchRewrite(std.testing.allocator, nc, 1, 5));
}

test "normalizeSource ternary rewrite is behavior-equivalent (contract diff)" {
    // The non-canonical `before` carries a ZTS612 hard error, so it never
    // extracts a contract. Normalize it to the canonical `match` form, then
    // prove that output is behaviorally equivalent to an independently
    // hand-written reference handler that expresses the same two response
    // paths with a `match`. Both are canonical, so both extract a contract;
    // `diffContracts` compares the observable behavior.
    const before =
        \\function handler(req: Request): Response {
        \\  const ok = req.method === "GET";
        \\  return ok ? Response.text("ready") : Response.text("not");
        \\}
    ;
    const reference =
        \\function handler(req: Request): Response {
        \\  const ok = req.method === "GET";
        \\  return match (!!ok) { when true: Response.text("ready"), default: Response.text("not") };
        \\}
    ;

    var nr = try normalizeSource(std.testing.allocator, before, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);

    var lhs = try precompile.runCheckOnlyFromSource(std.testing.allocator, nr.canonical_source, "handler.ts", null, true, null, false);
    defer lhs.deinit(std.testing.allocator);
    var rhs = try precompile.runCheckOnlyFromSource(std.testing.allocator, reference, "handler.ts", null, true, null, false);
    defer rhs.deinit(std.testing.allocator);

    const lhs_contract = lhs.contract orelse return error.NoContractFromCanonicalOutput;
    const rhs_contract = rhs.contract orelse return error.NoContractFromReference;

    var diff = try zigts.contract_diff.diffContracts(std.testing.allocator, &lhs_contract, &rhs_contract);
    defer diff.deinit(std.testing.allocator);
    try std.testing.expect(diff.behavioralVerdict().isSafeNoOp());
}

test "normalizeSource confluently rewrites a right-associative nested ternary" {
    // `a ? x : b ? y : z` is two ternaries; the normalizer resolves the inner
    // one first (post-order) and the outer one the next pass, reaching a unique
    // fixed point with both expressed as nested `match`.
    const source =
        \\function handler(req: Request): Response {
        \\  const a = req.method === "GET";
        \\  const b = req.method === "POST";
        \\  const status = a ? 200 : b ? 201 : 500;
        \\  return Response.json({ status });
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "?") == null);
    try std.testing.expect(std.mem.indexOf(
        u8,
        nr.canonical_source,
        "match (!!(a)) { when true: 200, default: match (!!(b)) { when true: 201, default: 500 } }",
    ) != null);

    // Idempotence: the normalized output is a fixed point.
    var again = try normalizeSource(std.testing.allocator, nr.canonical_source, "handler.ts");
    defer again.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings(nr.canonical_source, again.canonical_source);
    try std.testing.expectEqual(@as(u32, 0), again.iterations);
}

test "normalizeSource ternary rewrite preserves strings containing ? and :" {
    const source =
        \\function handler(req: Request): Response {
        \\  const ok = req.method === "GET";
        \\  const msg = ok ? "yes? a:b" : "no:x?y";
        \\  return Response.text(msg);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "when true: \"yes? a:b\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "default: \"no:x?y\"") != null);
}

test "normalizeSource ternary inside an object-literal value is rewritten in place" {
    const source =
        \\function handler(req: Request): Response {
        \\  const ok = req.method === "GET";
        \\  return Response.json({ code: ok ? 200 : 500, ok });
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expect(std.mem.indexOf(
        u8,
        nr.canonical_source,
        "code: match (!!(ok)) { when true: 200, default: 500 }, ok",
    ) != null);
}

// ---------------------------------------------------------------------------
// ZTS615 complex template interpolation -> hoisted const (span-keyed)
// ---------------------------------------------------------------------------

test "templateHoistRewrite hoists a single complex interpolation" {
    const source = "  const g = `Hi ${u.up()}!`;\n";
    // The template literal opens at the backtick on column 13.
    var rw = try templateHoistRewrite(std.testing.allocator, source, 1, 13);
    defer rw.deinit(std.testing.allocator);
    try std.testing.expectEqual(RepairIntent.name_const_above_template, rw.intent);
    // The replacement hoists the call into a const above and interpolates the
    // generated name; the name is derived from the byte offset of the `${`.
    const off = std.mem.indexOf(u8, source, "${").?; // offset of `$`
    const expected = try std.fmt.allocPrint(
        std.testing.allocator,
        "  const __zt_{d} = u.up();\n  const g = `Hi ${{__zt_{d}}}!`;",
        .{ off, off },
    );
    defer std.testing.allocator.free(expected);
    try std.testing.expectEqualStrings(expected, rw.replacement);
}

test "templateHoistRewrite refuses a template with only simple interpolations" {
    const source = "  const g = `Hi ${name} and ${a.b.c}`;\n";
    try std.testing.expectError(error.UnsupportedRefactor, templateHoistRewrite(std.testing.allocator, source, 1, 13));
}

test "templateHoistRewrite refuses a template inside a same-line arrow function" {
    // Hoisting above the line would move `n.toUpperCase()` outside the arrow,
    // where `n` is unbound (or a different outer binding).
    const source = "  const rows = names.map((n) => `Row ${n.toUpperCase()}`);\n";
    const bt = std.mem.indexOfScalar(u8, source, '`').?;
    try std.testing.expectError(
        error.UnsupportedRefactor,
        templateHoistRewrite(std.testing.allocator, source, 1, @intCast(bt + 1)),
    );
}

test "templateHoistRewrite refuses a template inside a same-line function body" {
    const source = "  function greet(u) { return `Hi ${u.up()}`; }\n";
    const bt = std.mem.indexOfScalar(u8, source, '`').?;
    try std.testing.expectError(
        error.UnsupportedRefactor,
        templateHoistRewrite(std.testing.allocator, source, 1, @intCast(bt + 1)),
    );
}

test "templateHoistRewrite still hoists when `function` only prefixes an identifier" {
    const source = "  const functionalGreeting = `Hi ${u.up()}!`;\n";
    const bt = std.mem.indexOfScalar(u8, source, '`').?;
    var rw = try templateHoistRewrite(std.testing.allocator, source, 1, @intCast(bt + 1));
    defer rw.deinit(std.testing.allocator);
    try std.testing.expectEqual(RepairIntent.name_const_above_template, rw.intent);
}

test "normalizeSource hoists a complex template interpolation and is fully canonical" {
    const source =
        \\function handler(req: Request): Response {
        \\  const name = req.headers["x-name"];
        \\  const greeting = `Hello, ${name.toUpperCase()}!`;
        \\  return Response.text(greeting);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, " = name.toUpperCase();") != null);
    // The template now interpolates a bare generated identifier, not a call.
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, ".toUpperCase()}") == null);

    var found = false;
    for (nr.rewrite_trace.items) |intent| {
        if (intent == .name_const_above_template) found = true;
    }
    try std.testing.expect(found);
}

test "normalizeSource hoists multiple complex interpolations in source order, idempotently" {
    const source =
        \\function handler(req: Request): Response {
        \\  const a = req.headers["a"];
        \\  const b = req.headers["b"];
        \\  const s = `${a.toUpperCase()} and ${b.toLowerCase()}`;
        \\  return Response.text(s);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);
    // Two hoisted consts, the first for `a.toUpperCase()` (earlier in source).
    const first = std.mem.indexOf(u8, nr.canonical_source, " = a.toUpperCase();") orelse return error.MissingFirstHoist;
    const second = std.mem.indexOf(u8, nr.canonical_source, " = b.toLowerCase();") orelse return error.MissingSecondHoist;
    try std.testing.expect(first < second);

    var again = try normalizeSource(std.testing.allocator, nr.canonical_source, "handler.ts");
    defer again.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings(nr.canonical_source, again.canonical_source);
    try std.testing.expectEqual(@as(u32, 0), again.iterations);
}

test "normalizeSource template hoist is behavior-equivalent (contract diff)" {
    // The non-canonical `before` carries a ZTS615 hard error, so it never
    // extracts a contract. Normalize it, then prove the hoisted output is
    // behaviorally equivalent to an independently hand-written reference that
    // names the interpolation in an explicit `const`.
    const before =
        \\function handler(req: Request): Response {
        \\  const name = req.headers["x-name"];
        \\  const greeting = `Hello, ${name.toUpperCase()}!`;
        \\  return Response.text(greeting);
        \\}
    ;
    const reference =
        \\function handler(req: Request): Response {
        \\  const name = req.headers["x-name"];
        \\  const upper = name.toUpperCase();
        \\  const greeting = `Hello, ${upper}!`;
        \\  return Response.text(greeting);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, before, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.fully_canonical);

    var lhs = try precompile.runCheckOnlyFromSource(std.testing.allocator, nr.canonical_source, "handler.ts", null, true, null, false);
    defer lhs.deinit(std.testing.allocator);
    var rhs = try precompile.runCheckOnlyFromSource(std.testing.allocator, reference, "handler.ts", null, true, null, false);
    defer rhs.deinit(std.testing.allocator);

    const lhs_contract = lhs.contract orelse return error.NoContractFromCanonicalOutput;
    const rhs_contract = rhs.contract orelse return error.NoContractFromReference;

    var diff = try zigts.contract_diff.diffContracts(std.testing.allocator, &lhs_contract, &rhs_contract);
    defer diff.deinit(std.testing.allocator);
    try std.testing.expect(diff.behavioralVerdict().isSafeNoOp());
}

test "normalizeSource refuses to hoist a multi-line template (left as residual)" {
    // The line-local hoist deliberately only handles single-line statements; a
    // template that wraps across lines is refused and stays a flagged ZTS615
    // hard error rather than risk an unsound splice.
    const source = "function handler(req: Request): Response {\n  const g = `a ${req.x.toUpperCase()}\nb`;\n  return Response.text(g);\n}\n";
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(!nr.fully_canonical);
    try std.testing.expect(nr.residual >= 1);
}

test "normalizeSource refuses to hoist inside a single-line function (left as residual)" {
    // The interpolation references the function's parameter `n`; a hoisted
    // const above the line would sit outside the function where `n` is
    // unbound (or a different outer binding).
    const source =
        \\function greet(n: string): string { return `Row ${n.toUpperCase()}`; }
        \\function handler(req: Request): Response {
        \\  return Response.text(greet("a"));
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(!nr.fully_canonical);
    try std.testing.expect(nr.residual >= 1);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "`Row ${n.toUpperCase()}`") != null);
}

test "normalizeSource reports dynamic computed access as residual diagnostic" {
    const source =
        \\function handler(req: Request): Response {
        \\  const key = req.headers["x-key"];
        \\  const value = req.headers[key];
        \\  return Response.text(value);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(!nr.fully_canonical);
    try std.testing.expect(nr.residual >= 1);

    var found = false;
    for (nr.residual_diagnostics.items) |diag| {
        if (std.mem.eql(u8, diag.code, "ZTS605")) {
            found = true;
            try std.testing.expect(diag.repair_intent == null);
        }
    }
    try std.testing.expect(found);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .fromArrayList(std.testing.allocator, &buf);
    try writeNormalizeJson(&aw.writer, "handler.ts", &nr, false);
    buf = aw.toArrayList();
    const json = try buf.toOwnedSlice(std.testing.allocator);
    defer std.testing.allocator.free(json);

    var parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json, .{});
    defer parsed.deinit();
    const residual = (parsed.value.object.get("residualDiagnostics") orelse return error.MissingResidualDiagnostics).array;
    try std.testing.expect(residual.items.len >= 1);
    try std.testing.expectEqualStrings("ZTS605", residual.items[0].object.get("code").?.string);
}

test "normalizeSource lifts a simple default parameter into the body" {
    const source =
        \\function greet(name = "world") {
        \\  return name;
        \\}
        \\function handler(req) {
        \\  const msg = greet(undefined);
        \\  return Response.text(msg);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.js");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "function greet(name) {") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "if (name === undefined) { name = \"world\"; }") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "function greet(name =") == null);

    var found = false;
    for (nr.rewrite_trace.items) |intent| {
        if (intent == .lift_default_to_body) found = true;
    }
    try std.testing.expect(found);
}

test "normalizeSource preserves default-parameter guard order" {
    const source =
        \\function pick(a = "first", b = a) {
        \\  return b;
        \\}
        \\function handler(req) {
        \\  return Response.text(pick(undefined, undefined));
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.js");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);

    const first = std.mem.indexOf(u8, nr.canonical_source, "if (a === undefined)") orelse return error.MissingFirstDefaultGuard;
    const second = std.mem.indexOf(u8, nr.canonical_source, "if (b === undefined)") orelse return error.MissingSecondDefaultGuard;
    try std.testing.expect(first < second);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "function pick(a =") == null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, ", b =") == null);
}

test "normalizeSource flattens a simple nested object destructure" {
    const source =
        \\function handler(req: Request): Response {
        \\  const payload = { user: { name: "ada" } };
        \\  const {user: {name}} = payload;
        \\  return Response.text(name);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "const {user} = payload;") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "const {name} = user;") != null);

    var found = false;
    for (nr.rewrite_trace.items) |intent| {
        if (intent == .flatten_destructure) found = true;
    }
    try std.testing.expect(found);
}

test "normalizeSource refuses nested destructure flattening that would shadow a live binding" {
    const source =
        \\const user = "global";
        \\function handler(req: Request): Response {
        \\  const payload = { user: { name: "ada" } };
        \\  const {user: {name}} = payload;
        \\  return Response.text(user + name);
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(!nr.fully_canonical);
    try std.testing.expect(nr.residual >= 1);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "const {user: {name}} = payload;") != null);
}

test "normalizeSource drops an unused entries index alias" {
    const source =
        \\function handler(req: Request): Response {
        \\  const items = ["a", "b"];
        \\  for (const pair of items.entries()) {
        \\    const [_i, item] = pair;
        \\    Response.text(item);
        \\  }
        \\  return Response.text("done");
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(nr.fully_canonical);
    try std.testing.expectEqual(@as(u32, 0), nr.residual);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "for (const item of items) {") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "const [_i, item]") == null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, ".entries()") == null);

    var found = false;
    for (nr.rewrite_trace.items) |intent| {
        if (intent == .drop_unused_index_alias) found = true;
    }
    try std.testing.expect(found);
}

test "normalizeSource refuses entries alias rewrite when pair binding is still read" {
    const source =
        \\function handler(req: Request): Response {
        \\  const items = ["a", "b"];
        \\  for (const pair of items.entries()) {
        \\    const [_i, item] = pair;
        \\    Response.text(pair[1]);
        \\  }
        \\  return Response.text("done");
        \\}
    ;
    var nr = try normalizeSource(std.testing.allocator, source, "handler.ts");
    defer nr.deinit(std.testing.allocator);
    try std.testing.expect(nr.converged);
    try std.testing.expect(!nr.fully_canonical);
    try std.testing.expect(nr.residual >= 1);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "for (const pair of items.entries())") != null);
    try std.testing.expect(std.mem.indexOf(u8, nr.canonical_source, "Response.text(pair[1]);") != null);
}

test "compoundAssignReplacement parenthesizes a compound rhs to preserve precedence" {
    const allocator = std.testing.allocator;

    // A single atom needs no parens (and must not gain any, to match the
    // canonical form and the existing repair tests).
    {
        const out = try compoundAssignReplacement(allocator, "  n += 1;");
        defer allocator.free(out);
        try std.testing.expectEqualStrings("  n = n + 1;", out);
    }
    {
        const out = try compoundAssignReplacement(allocator, "  state.total *= factor;");
        defer allocator.free(out);
        try std.testing.expectEqualStrings("  state.total = state.total * factor;", out);
    }

    // A compound rhs MUST be parenthesized: without parens this reassociates to
    // (total - fee) + tax and silently changes the value.
    {
        const out = try compoundAssignReplacement(allocator, "  total -= fee + tax;");
        defer allocator.free(out);
        try std.testing.expectEqualStrings("  total = total - (fee + tax);", out);
    }
    {
        const out = try compoundAssignReplacement(allocator, "  x /= a * b;");
        defer allocator.free(out);
        try std.testing.expectEqualStrings("  x = x / (a * b);", out);
    }
    // No trailing semicolon: still parenthesized.
    {
        const out = try compoundAssignReplacement(allocator, "  x -= a - b");
        defer allocator.free(out);
        try std.testing.expectEqualStrings("  x = x - (a - b)", out);
    }
    // A trailing comment defeats the textual split; refuse rather than mangle.
    try std.testing.expectError(error.UnsupportedRefactor, compoundAssignReplacement(allocator, "  n += a + b; // note"));
}

test "delimitersBalanced detects expressions that do not finish on the line" {
    try std.testing.expect(delimitersBalanced("makeUser(1, 2)"));
    try std.testing.expect(delimitersBalanced("payload"));
    try std.testing.expect(delimitersBalanced("{ a: 1 };"));
    // A delimiter inside a string literal does not count toward the balance.
    try std.testing.expect(delimitersBalanced("f(\")\")"));
    // Unbalanced openers (a multiline RHS) and unterminated strings are refused.
    try std.testing.expect(!delimitersBalanced("makeUser("));
    try std.testing.expect(!delimitersBalanced("{"));
    try std.testing.expect(!delimitersBalanced("f(\"unterminated"));
}

test "typeIsAlreadyOptional matches only an exact top-level undefined member" {
    try std.testing.expect(typeIsAlreadyOptional("undefined"));
    try std.testing.expect(typeIsAlreadyOptional("string | undefined"));
    try std.testing.expect(typeIsAlreadyOptional("undefined | string"));
    // A substring of an identifier is not a match (the prior bug).
    try std.testing.expect(!typeIsAlreadyOptional("undefinedish"));
    try std.testing.expect(!typeIsAlreadyOptional("string | undefinedValue"));
    // A nested optional is not a top-level optional.
    try std.testing.expect(!typeIsAlreadyOptional("Array<T | undefined>"));
    try std.testing.expect(!typeIsAlreadyOptional("number"));
}

test "nestedDestructureRewrite refuses a multiline right-hand side" {
    const source =
        \\function handler(req: Request): Response {
        \\  const {user: {name}} = makeUser(
        \\    1,
        \\  );
        \\  return Response.json({ name });
        \\}
    ;
    try std.testing.expectError(
        error.UnsupportedRefactor,
        nestedDestructureRewrite(std.testing.allocator, source, 2, 3),
    );
}

test "nestedDestructureRewrite flattens a single-line nested pattern" {
    const source =
        \\function handler(req: Request): Response {
        \\  const {user: {name}} = makeUser(1);
        \\  return Response.json({ name });
        \\}
    ;
    var rw = try nestedDestructureRewrite(std.testing.allocator, source, 2, 3);
    defer rw.deinit(std.testing.allocator);
    try std.testing.expect(std.mem.indexOf(u8, rw.replacement, "const {user} = makeUser(1);") != null);
    try std.testing.expect(std.mem.indexOf(u8, rw.replacement, "const {name} = user;") != null);
}
