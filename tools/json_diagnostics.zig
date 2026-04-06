//! Structured JSON Diagnostic Output
//!
//! Converts internal diagnostics from the parser, BoolChecker, TypeChecker,
//! and HandlerVerifier into machine-readable JSON for the compiler-in-the-loop
//! workflow. Claude Code calls `zts check --json` and parses these diagnostics
//! to fix handler code using the `suggestion` field.
//!
//! Architecture: each checker's Diagnostic -> JsonDiagnostic -> JSON on stdout

const std = @import("std");
const zigts = @import("zigts");
const parser = zigts.parser;
const bool_checker = zigts.bool_checker;
const type_checker = zigts.type_checker;
const handler_verifier = zigts.handler_verifier;
const handler_contract = zigts.handler_contract;
const writeJsonString = handler_contract.writeJsonString;

pub const IrView = parser.IrView;
pub const SourceLocation = parser.SourceLocation;
pub const ParseError = parser.ParseError;
pub const ErrorKind = parser.ErrorKind;

// -------------------------------------------------------------------------
// Structured diagnostic
// -------------------------------------------------------------------------

pub const JsonDiagnostic = struct {
    code: []const u8,
    severity: []const u8,
    message: []const u8,
    file: []const u8,
    line: u32,
    column: u16,
    suggestion: ?[]const u8,
};

// -------------------------------------------------------------------------
// Diagnostic code mapping
// -------------------------------------------------------------------------

/// Parser error codes: ZTS0xx
fn parserErrorCode(kind: ErrorKind) []const u8 {
    return switch (kind) {
        .unsupported_feature => "ZTS001",
        .unexpected_token => "ZTS002",
        .expected_token => "ZTS003",
        .expected_expression => "ZTS004",
        .expected_statement => "ZTS005",
        .expected_identifier => "ZTS006",
        .unexpected_eof => "ZTS007",
        .unterminated_string => "ZTS008",
        .unterminated_template => "ZTS009",
        .unterminated_regex => "ZTS010",
        .unterminated_comment => "ZTS011",
        .invalid_number => "ZTS012",
        .invalid_escape_sequence => "ZTS013",
        .invalid_unicode_escape => "ZTS014",
        .expected_property_name => "ZTS015",
        .invalid_assignment_target => "ZTS016",
        .invalid_destructuring => "ZTS017",
        .duplicate_parameter => "ZTS018",
        .duplicate_binding => "ZTS019",
        .undeclared_variable => "ZTS020",
        .const_without_initializer => "ZTS021",
        .invalid_break => "ZTS022",
        .invalid_continue => "ZTS023",
        .invalid_return => "ZTS024",
        .invalid_yield => "ZTS025",
        .invalid_await => "ZTS026",
        .invalid_super => "ZTS027",
        .too_many_parameters => "ZTS028",
        .too_many_locals => "ZTS029",
        .too_many_upvalues => "ZTS030",
        .too_many_constants => "ZTS031",
        .jump_too_large => "ZTS032",
        .mismatched_jsx_tag => "ZTS033",
        .invalid_jsx_attribute => "ZTS034",
        .unclosed_jsx_element => "ZTS035",
        .jsx_expression_expected => "ZTS036",
        .invalid_import => "ZTS037",
        .invalid_export => "ZTS038",
        .duplicate_export => "ZTS039",
        .unexpected_character => "ZTS040",
    };
}

/// BoolChecker error codes: ZTS1xx
fn boolCheckerCode(kind: bool_checker.DiagnosticKind) []const u8 {
    return switch (kind) {
        .condition_not_boolean => "ZTS100",
        .logical_operand_not_boolean => "ZTS101",
        .not_operand_not_boolean => "ZTS102",
        .nullish_on_non_nullable => "ZTS103",
        .arithmetic_on_non_numeric => "ZTS104",
        .mixed_type_add => "ZTS105",
        .add_on_non_addable => "ZTS106",
        .tautological_comparison => "ZTS107",
    };
}

/// TypeChecker error codes: ZTS2xx
fn typeCheckerCode(kind: type_checker.DiagnosticKind) []const u8 {
    return switch (kind) {
        .type_mismatch => "ZTS200",
        .missing_field => "ZTS201",
        .arg_count_mismatch => "ZTS202",
        .arg_type_mismatch => "ZTS203",
        .return_type_mismatch => "ZTS204",
        .non_exhaustive_match => "ZTS205",
    };
}

/// HandlerVerifier error codes: ZTS3xx
fn verifierCode(kind: handler_verifier.DiagnosticKind) []const u8 {
    return switch (kind) {
        .missing_return_else => "ZTS300",
        .missing_return_default => "ZTS301",
        .missing_return_path => "ZTS302",
        .unchecked_result_value => "ZTS303",
        .unreachable_after_return => "ZTS304",
        .unused_variable => "ZTS305",
        .unused_import => "ZTS306",
        .non_exhaustive_match => "ZTS307",
        .unchecked_optional_use => "ZTS308",
        .unchecked_optional_access => "ZTS309",
        .module_scope_mutation => "ZTS310",
    };
}

// -------------------------------------------------------------------------
// Conversion helpers
// -------------------------------------------------------------------------

/// Extract suggestion from unsupported_feature parser errors.
/// These follow the pattern: "'X' is not supported; use Y instead"
fn splitSuggestion(message: []const u8) struct { msg: []const u8, suggestion: ?[]const u8 } {
    if (std.mem.indexOf(u8, message, "; ")) |sep| {
        return .{
            .msg = message[0..sep],
            .suggestion = message[sep + 2 ..],
        };
    }
    return .{ .msg = message, .suggestion = null };
}

pub fn fromParseError(err: ParseError, file: []const u8) JsonDiagnostic {
    const code = parserErrorCode(err.kind);

    if (err.kind == .unsupported_feature) {
        const parts = splitSuggestion(err.message);
        return .{
            .code = code,
            .severity = "error",
            .message = parts.msg,
            .file = file,
            .line = err.location.line,
            .column = err.location.column,
            .suggestion = parts.suggestion,
        };
    }

    return .{
        .code = code,
        .severity = "error",
        .message = err.message,
        .file = file,
        .line = err.location.line,
        .column = err.location.column,
        .suggestion = err.expected,
    };
}

pub fn fromBoolDiagnostic(diag: bool_checker.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    const loc = ir_view.getLoc(diag.node) orelse return null;
    return .{
        .code = boolCheckerCode(diag.kind),
        .severity = diag.severity.label(),
        .message = diag.message,
        .file = file,
        .line = loc.line,
        .column = loc.column,
        .suggestion = diag.help,
    };
}

pub fn fromTypeDiagnostic(diag: type_checker.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    const loc = ir_view.getLoc(diag.node) orelse return null;
    return .{
        .code = typeCheckerCode(diag.kind),
        .severity = diag.severity.label(),
        .message = diag.message,
        .file = file,
        .line = loc.line,
        .column = loc.column,
        .suggestion = diag.help,
    };
}

pub fn fromVerifierDiagnostic(diag: handler_verifier.Diagnostic, ir_view: IrView, file: []const u8) ?JsonDiagnostic {
    const loc = ir_view.getLoc(diag.node) orelse return null;
    return .{
        .code = verifierCode(diag.kind),
        .severity = diag.severity.label(),
        .message = diag.message,
        .file = file,
        .line = loc.line,
        .column = loc.column,
        .suggestion = diag.help,
    };
}

// -------------------------------------------------------------------------
// JSON serialization
// -------------------------------------------------------------------------

pub fn writeDiagnosticJson(writer: anytype, diag: *const JsonDiagnostic) !void {
    try writer.writeAll("{\"code\":");
    try writeJsonString(writer, diag.code);
    try writer.writeAll(",\"severity\":");
    try writeJsonString(writer, diag.severity);
    try writer.writeAll(",\"message\":");
    try writeJsonString(writer, diag.message);
    try writer.writeAll(",\"file\":");
    try writeJsonString(writer, diag.file);
    try writer.print(",\"line\":{d},\"column\":{d}", .{ diag.line, diag.column });
    if (diag.suggestion) |s| {
        try writer.writeAll(",\"suggestion\":");
        try writeJsonString(writer, s);
    } else {
        try writer.writeAll(",\"suggestion\":null");
    }
    try writer.writeByte('}');
}

fn writeDiagnosticsArray(writer: anytype, diagnostics: []const JsonDiagnostic) !void {
    try writer.writeByte('[');
    for (diagnostics, 0..) |*diag, i| {
        if (i > 0) try writer.writeByte(',');
        try writeDiagnosticJson(writer, diag);
    }
    try writer.writeByte(']');
}

/// Emit success JSON with proof summary and optional warnings.
pub fn writeSuccessJson(
    writer: anytype,
    contract: ?*const handler_contract.HandlerContract,
    diagnostics: []const JsonDiagnostic,
) !void {
    try writer.writeAll("{\"success\":true");

    // Proof section from contract
    if (contract) |c| {
        try writer.writeAll(",\"proof\":{");

        // Env vars
        try writer.writeAll("\"env_vars\":[");
        for (c.env.literal.items, 0..) |v, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, v);
        }
        try writer.writeByte(']');

        // Outbound hosts
        try writer.writeAll(",\"outbound_hosts\":[");
        for (c.egress.hosts.items, 0..) |h, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, h);
        }
        try writer.writeByte(']');

        // Virtual modules
        try writer.writeAll(",\"virtual_modules\":[");
        for (c.modules.items, 0..) |m, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonString(writer, m);
        }
        try writer.writeByte(']');

        // Properties
        if (c.properties) |props| {
            try writer.writeAll(",\"properties\":{");
            try writer.print("\"retry_safe\":{},\"idempotent\":{},\"injection_safe\":{},\"deterministic\":{},\"read_only\":{},\"state_isolated\":{},\"fault_covered\":{}", .{
                props.retry_safe,
                props.idempotent,
                props.injection_safe,
                props.deterministic,
                props.read_only,
                props.state_isolated,
                props.fault_covered,
            });
            try writer.writeByte('}');
        }

        try writer.writeByte('}');
    }

    // Diagnostics (warnings only for success)
    try writer.writeAll(",\"diagnostics\":");
    try writeDiagnosticsArray(writer, diagnostics);

    try writer.writeAll("}\n");
}

/// Emit error JSON with diagnostics array.
pub fn writeErrorJson(writer: anytype, diagnostics: []const JsonDiagnostic) !void {
    try writer.writeAll("{\"success\":false,\"diagnostics\":");
    try writeDiagnosticsArray(writer, diagnostics);
    try writer.writeAll("}\n");
}

// -------------------------------------------------------------------------
// Module listing
// -------------------------------------------------------------------------

pub fn writeModulesJson(writer: anytype) !void {
    const builtin_modules = zigts.builtin_modules;
    try writer.writeByte('[');
    for (builtin_modules.all, 0..) |binding, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"specifier\":");
        try writeJsonString(writer, binding.specifier);
        try writer.writeAll(",\"exports\":[");
        for (binding.exports, 0..) |func, j| {
            if (j > 0) try writer.writeByte(',');
            try writeJsonString(writer, func.name);
        }
        try writer.writeAll("]}");
    }
    try writer.writeAll("]\n");
}

pub fn writeModulesText(writer: anytype) !void {
    const builtin_modules = zigts.builtin_modules;
    for (builtin_modules.all) |binding| {
        try writer.print("{s}\n", .{binding.specifier});
        for (binding.exports) |func| {
            try writer.print("  {s}\n", .{func.name});
        }
    }
}

// -------------------------------------------------------------------------
// Features listing
// -------------------------------------------------------------------------

const FeatureStatus = enum {
    allowed,
    blocked,

    fn label(self: FeatureStatus) []const u8 {
        return switch (self) {
            .allowed => "allowed",
            .blocked => "blocked",
        };
    }
};

const Feature = struct {
    name: []const u8,
    status: FeatureStatus,
    alternative: ?[]const u8,
};

const features = [_]Feature{
    // Allowed
    .{ .name = "const", .status = .allowed, .alternative = null },
    .{ .name = "let", .status = .allowed, .alternative = null },
    .{ .name = "function", .status = .allowed, .alternative = null },
    .{ .name = "arrow functions", .status = .allowed, .alternative = null },
    .{ .name = "destructuring", .status = .allowed, .alternative = null },
    .{ .name = "spread/rest", .status = .allowed, .alternative = null },
    .{ .name = "template literals", .status = .allowed, .alternative = null },
    .{ .name = "if/else", .status = .allowed, .alternative = null },
    .{ .name = "for...of", .status = .allowed, .alternative = null },
    .{ .name = "ternary", .status = .allowed, .alternative = null },
    .{ .name = "optional chaining", .status = .allowed, .alternative = null },
    .{ .name = "nullish coalescing", .status = .allowed, .alternative = null },
    .{ .name = "match expression", .status = .allowed, .alternative = null },
    .{ .name = "assert statement", .status = .allowed, .alternative = null },
    .{ .name = "pipe operator", .status = .allowed, .alternative = null },
    .{ .name = "import/export", .status = .allowed, .alternative = null },
    .{ .name = "type annotations", .status = .allowed, .alternative = null },
    .{ .name = "distinct type", .status = .allowed, .alternative = null },
    .{ .name = "readonly fields", .status = .allowed, .alternative = null },
    .{ .name = "type guards (x is T)", .status = .allowed, .alternative = null },
    .{ .name = "template literal types", .status = .allowed, .alternative = null },
    .{ .name = "comptime()", .status = .allowed, .alternative = null },
    // Blocked
    .{ .name = "switch/case", .status = .blocked, .alternative = "use 'match' expression" },
    .{ .name = "var", .status = .blocked, .alternative = "use 'let' or 'const'" },
    .{ .name = "class", .status = .blocked, .alternative = "use plain objects and functions" },
    .{ .name = "while", .status = .blocked, .alternative = "use 'for...of' with a finite collection" },
    .{ .name = "do...while", .status = .blocked, .alternative = "use 'for...of' with a finite collection" },
    .{ .name = "for(;;)", .status = .blocked, .alternative = "use 'for (const i of range(n))'" },
    .{ .name = "for...in", .status = .blocked, .alternative = "use 'for (const k of Object.keys(obj))'" },
    .{ .name = "try/catch", .status = .blocked, .alternative = "use Result types and check .ok" },
    .{ .name = "throw", .status = .blocked, .alternative = "return an error Response" },
    .{ .name = "async/await", .status = .blocked, .alternative = "use fetchSync(), parallel(), race()" },
    .{ .name = "new", .status = .blocked, .alternative = "use factory functions or object literals" },
    .{ .name = "this", .status = .blocked, .alternative = "use explicit parameter passing" },
    .{ .name = "null", .status = .blocked, .alternative = "use undefined" },
    .{ .name = "== / !=", .status = .blocked, .alternative = "use === / !==" },
    .{ .name = "++ / --", .status = .blocked, .alternative = "use x = x + 1" },
    .{ .name = "regex", .status = .blocked, .alternative = "use string methods (includes, startsWith, etc.)" },
    .{ .name = "delete", .status = .blocked, .alternative = "use destructuring: const { key, ...rest } = obj" },
    .{ .name = "enum", .status = .blocked, .alternative = "use object literals or discriminated unions" },
    .{ .name = "decorator (@)", .status = .blocked, .alternative = "use function composition" },
    .{ .name = "namespace", .status = .blocked, .alternative = "use ES6 modules" },
};

pub fn writeFeaturesJson(writer: anytype) !void {
    try writer.writeByte('[');
    for (features, 0..) |f, i| {
        if (i > 0) try writer.writeByte(',');
        try writer.writeAll("{\"name\":");
        try writeJsonString(writer, f.name);
        try writer.writeAll(",\"status\":");
        try writeJsonString(writer, f.status.label());
        if (f.alternative) |alt| {
            try writer.writeAll(",\"alternative\":");
            try writeJsonString(writer, alt);
        } else {
            try writer.writeAll(",\"alternative\":null");
        }
        try writer.writeByte('}');
    }
    try writer.writeAll("]\n");
}

pub fn writeFeaturesText(writer: anytype) !void {
    try writer.writeAll("Allowed:\n");
    for (features) |f| {
        if (f.status == .allowed) {
            try writer.print("  {s}\n", .{f.name});
        }
    }
    try writer.writeAll("\nBlocked:\n");
    for (features) |f| {
        if (f.status == .blocked) {
            if (f.alternative) |alt| {
                try writer.print("  {s} - {s}\n", .{ f.name, alt });
            } else {
                try writer.print("  {s}\n", .{f.name});
            }
        }
    }
}

// -------------------------------------------------------------------------
// Tests
// -------------------------------------------------------------------------

test "splitSuggestion: unsupported feature message" {
    const result = splitSuggestion("'while' is not supported; use 'for-of' with a finite collection instead");
    try std.testing.expectEqualStrings("'while' is not supported", result.msg);
    try std.testing.expectEqualStrings("use 'for-of' with a finite collection instead", result.suggestion.?);
}

test "splitSuggestion: no semicolon" {
    const result = splitSuggestion("unexpected token");
    try std.testing.expectEqualStrings("unexpected token", result.msg);
    try std.testing.expect(result.suggestion == null);
}

test "parserErrorCode maps all kinds" {
    // Spot check a few
    try std.testing.expectEqualStrings("ZTS001", parserErrorCode(.unsupported_feature));
    try std.testing.expectEqualStrings("ZTS002", parserErrorCode(.unexpected_token));
    try std.testing.expectEqualStrings("ZTS037", parserErrorCode(.invalid_import));
}

test "boolCheckerCode maps all kinds" {
    try std.testing.expectEqualStrings("ZTS100", boolCheckerCode(.condition_not_boolean));
    try std.testing.expectEqualStrings("ZTS105", boolCheckerCode(.mixed_type_add));
}

test "typeCheckerCode maps all kinds" {
    try std.testing.expectEqualStrings("ZTS200", typeCheckerCode(.type_mismatch));
    try std.testing.expectEqualStrings("ZTS202", typeCheckerCode(.arg_count_mismatch));
}

test "verifierCode maps all kinds" {
    try std.testing.expectEqualStrings("ZTS300", verifierCode(.missing_return_else));
    try std.testing.expectEqualStrings("ZTS303", verifierCode(.unchecked_result_value));
    try std.testing.expectEqualStrings("ZTS305", verifierCode(.unused_variable));
    try std.testing.expectEqualStrings("ZTS310", verifierCode(.module_scope_mutation));
}

test "fromParseError: unsupported feature extracts suggestion" {
    const err = ParseError{
        .kind = .unsupported_feature,
        .location = .{ .line = 23, .column = 3, .offset = 100 },
        .message = "'try/catch' is not supported; use Result types for error handling",
        .token_text = null,
        .expected = null,
    };
    const diag = fromParseError(err, "handler.ts");
    try std.testing.expectEqualStrings("ZTS001", diag.code);
    try std.testing.expectEqualStrings("error", diag.severity);
    try std.testing.expectEqualStrings("'try/catch' is not supported", diag.message);
    try std.testing.expectEqualStrings("handler.ts", diag.file);
    try std.testing.expectEqual(@as(u32, 23), diag.line);
    try std.testing.expectEqual(@as(u16, 3), diag.column);
    try std.testing.expectEqualStrings("use Result types for error handling", diag.suggestion.?);
}

test "fromParseError: expected token uses expected field" {
    const err = ParseError{
        .kind = .expected_token,
        .location = .{ .line = 5, .column = 10, .offset = 40 },
        .message = "unexpected token",
        .token_text = null,
        .expected = "';'",
    };
    const diag = fromParseError(err, "handler.ts");
    try std.testing.expectEqualStrings("ZTS003", diag.code);
    try std.testing.expectEqualStrings("';'", diag.suggestion.?);
}
