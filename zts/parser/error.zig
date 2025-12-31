//! Error handling for the JavaScript parser
//!
//! Provides structured error collection with source locations
//! for better error messages and multi-error reporting.

const std = @import("std");
const token = @import("token.zig");

pub const SourceLocation = token.SourceLocation;
pub const Token = token.Token;

/// Parse error kinds
pub const ErrorKind = enum {
    // Lexical errors
    unexpected_character,
    unterminated_string,
    unterminated_template,
    unterminated_regex,
    unterminated_comment,
    invalid_number,
    invalid_escape_sequence,
    invalid_unicode_escape,

    // Syntax errors
    unexpected_token,
    expected_token,
    expected_expression,
    expected_statement,
    expected_identifier,
    expected_property_name,
    unexpected_eof,
    invalid_assignment_target,
    invalid_destructuring,
    duplicate_parameter,

    // Semantic errors
    duplicate_binding,
    undeclared_variable,
    const_without_initializer,
    invalid_break,
    invalid_continue,
    invalid_return,
    invalid_yield,
    invalid_await,
    invalid_super,
    too_many_parameters,
    too_many_locals,
    too_many_upvalues,
    too_many_constants,
    jump_too_large,

    // JSX errors
    mismatched_jsx_tag,
    invalid_jsx_attribute,
    unclosed_jsx_element,
    jsx_expression_expected,

    // Module errors
    invalid_import,
    invalid_export,
    duplicate_export,

    // Unsupported features
    unsupported_feature,
};

/// A single parse error
pub const ParseError = struct {
    kind: ErrorKind,
    location: SourceLocation,
    message: []const u8,
    token_text: ?[]const u8,
    expected: ?[]const u8,

    /// Format error for display
    pub fn format(
        self: ParseError,
        source: []const u8,
        writer: anytype,
    ) !void {
        // Error header
        try writer.print("error: {s}\n", .{self.message});

        // Location
        try writer.print("  --> {}:{}\n", .{ self.location.line, self.location.column });

        // Source context
        if (self.getSourceLine(source)) |line| {
            try writer.print("   |\n", .{});
            try writer.print("{d: >3} | {s}\n", .{ self.location.line, line });
            try writer.print("   | ", .{});

            // Underline
            var col: u16 = 1;
            while (col < self.location.column) : (col += 1) {
                try writer.writeByte(' ');
            }
            try writer.writeAll("^\n");
        }

        // Additional context
        if (self.expected) |exp| {
            try writer.print("   = expected: {s}\n", .{exp});
        }
        if (self.token_text) |txt| {
            try writer.print("   = found: '{s}'\n", .{txt});
        }
    }

    fn getSourceLine(self: ParseError, source: []const u8) ?[]const u8 {
        var current_line: u32 = 1;
        var line_start: usize = 0;

        for (source, 0..) |c, i| {
            if (current_line == self.location.line) {
                var line_end = i;
                while (line_end < source.len and source[line_end] != '\n') {
                    line_end += 1;
                }
                return source[line_start..line_end];
            }
            if (c == '\n') {
                current_line += 1;
                line_start = i + 1;
            }
        }

        // Handle last line without newline
        if (current_line == self.location.line and line_start < source.len) {
            return source[line_start..];
        }

        return null;
    }
};

/// Error collector that accumulates multiple errors
pub const ErrorList = struct {
    errors: std.ArrayList(ParseError),
    allocator: std.mem.Allocator,
    source: []const u8,
    max_errors: usize,
    panic_mode: bool,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) ErrorList {
        return .{
            .errors = .empty,
            .allocator = allocator,
            .source = source,
            .max_errors = 20, // Stop after 20 errors
            .panic_mode = false,
        };
    }

    pub fn deinit(self: *ErrorList) void {
        self.errors.deinit(self.allocator);
    }

    /// Add an error
    pub fn addError(
        self: *ErrorList,
        kind: ErrorKind,
        loc: SourceLocation,
        message: []const u8,
    ) void {
        if (self.panic_mode) return; // Don't accumulate in panic mode
        if (self.errors.items.len >= self.max_errors) return;

        self.errors.append(self.allocator, .{
            .kind = kind,
            .location = loc,
            .message = message,
            .token_text = null,
            .expected = null,
        }) catch {};
    }

    /// Add an error with token context
    pub fn addErrorAt(
        self: *ErrorList,
        kind: ErrorKind,
        tok: Token,
        message: []const u8,
    ) void {
        if (self.panic_mode) return;
        if (self.errors.items.len >= self.max_errors) return;

        const token_text = if (tok.len > 0 and tok.len <= 20)
            tok.text(self.source)
        else
            null;

        self.errors.append(self.allocator, .{
            .kind = kind,
            .location = tok.location(),
            .message = message,
            .token_text = token_text,
            .expected = null,
        }) catch {};
    }

    /// Add an "expected X, found Y" error
    pub fn addExpectedError(
        self: *ErrorList,
        tok: Token,
        expected: []const u8,
    ) void {
        if (self.panic_mode) return;
        if (self.errors.items.len >= self.max_errors) return;

        const token_text = if (tok.len > 0 and tok.len <= 20)
            tok.text(self.source)
        else
            null;

        self.errors.append(self.allocator, .{
            .kind = .expected_token,
            .location = tok.location(),
            .message = "unexpected token",
            .token_text = token_text,
            .expected = expected,
        }) catch {};
    }

    /// Enter panic mode (suppress further errors until synchronization)
    pub fn enterPanicMode(self: *ErrorList) void {
        self.panic_mode = true;
    }

    /// Exit panic mode (after synchronizing to known good state)
    pub fn exitPanicMode(self: *ErrorList) void {
        self.panic_mode = false;
    }

    /// Check if any errors have been recorded
    pub fn hasErrors(self: *const ErrorList) bool {
        return self.errors.items.len > 0;
    }

    /// Get error count
    pub fn errorCount(self: *const ErrorList) usize {
        return self.errors.items.len;
    }

    /// Format a single error to a buffer (for simple output)
    pub fn formatFirstError(self: *const ErrorList, buf: []u8) []const u8 {
        if (self.errors.items.len == 0) return "";

        const err = self.errors.items[0];
        var pos: usize = 0;

        // Simple formatting: "error at line:col: message"
        const prefix = "error at ";
        if (pos + prefix.len < buf.len) {
            @memcpy(buf[pos..][0..prefix.len], prefix);
            pos += prefix.len;
        }

        // Format line number
        var line_buf: [16]u8 = undefined;
        const line_str = std.fmt.bufPrint(&line_buf, "{}:{}: ", .{ err.location.line, err.location.column }) catch return buf[0..pos];
        if (pos + line_str.len < buf.len) {
            @memcpy(buf[pos..][0..line_str.len], line_str);
            pos += line_str.len;
        }

        // Add message
        const msg_len = @min(err.message.len, buf.len - pos);
        if (msg_len > 0) {
            @memcpy(buf[pos..][0..msg_len], err.message[0..msg_len]);
            pos += msg_len;
        }

        return buf[0..pos];
    }

    /// Get errors as slice
    pub fn getErrors(self: *const ErrorList) []const ParseError {
        return self.errors.items;
    }
};

/// Error builder for common error patterns
pub const ErrorBuilder = struct {
    errors: *ErrorList,

    pub fn init(errors: *ErrorList) ErrorBuilder {
        return .{ .errors = errors };
    }

    pub fn unexpectedToken(self: ErrorBuilder, tok: Token) void {
        self.errors.addErrorAt(.unexpected_token, tok, "unexpected token");
    }

    pub fn expectedExpression(self: ErrorBuilder, tok: Token) void {
        self.errors.addExpectedError(tok, "expression");
    }

    pub fn expectedIdentifier(self: ErrorBuilder, tok: Token) void {
        self.errors.addExpectedError(tok, "identifier");
    }

    pub fn expectedSemicolon(self: ErrorBuilder, tok: Token) void {
        self.errors.addExpectedError(tok, "';'");
    }

    pub fn expectedColon(self: ErrorBuilder, tok: Token) void {
        self.errors.addExpectedError(tok, "':'");
    }

    pub fn expectedCloseParen(self: ErrorBuilder, tok: Token) void {
        self.errors.addExpectedError(tok, "')'");
    }

    pub fn expectedCloseBrace(self: ErrorBuilder, tok: Token) void {
        self.errors.addExpectedError(tok, "'}'");
    }

    pub fn expectedCloseBracket(self: ErrorBuilder, tok: Token) void {
        self.errors.addExpectedError(tok, "']'");
    }

    pub fn invalidAssignmentTarget(self: ErrorBuilder, loc: SourceLocation) void {
        self.errors.addError(.invalid_assignment_target, loc, "invalid assignment target");
    }

    pub fn duplicateBinding(self: ErrorBuilder, tok: Token, name: []const u8) void {
        _ = name;
        self.errors.addErrorAt(.duplicate_binding, tok, "identifier already declared in this scope");
    }

    pub fn constWithoutInit(self: ErrorBuilder, tok: Token) void {
        self.errors.addErrorAt(.const_without_initializer, tok, "const declaration must have an initializer");
    }

    pub fn invalidBreak(self: ErrorBuilder, tok: Token) void {
        self.errors.addErrorAt(.invalid_break, tok, "'break' outside of loop or switch");
    }

    pub fn invalidContinue(self: ErrorBuilder, tok: Token) void {
        self.errors.addErrorAt(.invalid_continue, tok, "'continue' outside of loop");
    }

    pub fn invalidReturn(self: ErrorBuilder, tok: Token) void {
        self.errors.addErrorAt(.invalid_return, tok, "'return' outside of function");
    }

    pub fn tooManyLocals(self: ErrorBuilder, loc: SourceLocation) void {
        self.errors.addError(.too_many_locals, loc, "too many local variables (max 255)");
    }

    pub fn tooManyUpvalues(self: ErrorBuilder, loc: SourceLocation) void {
        self.errors.addError(.too_many_upvalues, loc, "too many upvalues (max 255)");
    }

    pub fn unterminatedString(self: ErrorBuilder, loc: SourceLocation) void {
        self.errors.addError(.unterminated_string, loc, "unterminated string literal");
    }

    pub fn unterminatedTemplate(self: ErrorBuilder, loc: SourceLocation) void {
        self.errors.addError(.unterminated_template, loc, "unterminated template literal");
    }

    pub fn mismatchedJsxTag(self: ErrorBuilder, tok: Token, expected_tag: []const u8) void {
        _ = expected_tag;
        self.errors.addErrorAt(.mismatched_jsx_tag, tok, "JSX closing tag does not match opening tag");
    }

    pub fn unclosedJsxElement(self: ErrorBuilder, loc: SourceLocation) void {
        self.errors.addError(.unclosed_jsx_element, loc, "unclosed JSX element");
    }
};

// --- Tests ---

test "error formatting" {
    const source = "let x = ;\nlet y = 2;";
    var list = ErrorList.init(std.testing.allocator, source);
    defer list.deinit();

    list.addError(.expected_expression, .{ .line = 1, .column = 9, .offset = 8 }, "expected expression");

    try std.testing.expect(list.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), list.errorCount());

    // Test formatting (just verify it doesn't crash)
    var buf: [1024]u8 = undefined;
    const output = list.formatFirstError(&buf);
    try std.testing.expect(output.len > 0);
}

test "error builder patterns" {
    const source = "let 123 = x;";
    var list = ErrorList.init(std.testing.allocator, source);
    defer list.deinit();

    var builder = ErrorBuilder.init(&list);

    const tok = Token{
        .type = .number,
        .start = 4,
        .len = 3,
        .line = 1,
        .column = 5,
    };

    builder.expectedIdentifier(tok);

    try std.testing.expect(list.hasErrors());
    const err = list.getErrors()[0];
    try std.testing.expectEqual(ErrorKind.expected_token, err.kind);
    try std.testing.expectEqualStrings("identifier", err.expected.?);
}

test "panic mode suppresses errors" {
    const source = "x y z";
    var list = ErrorList.init(std.testing.allocator, source);
    defer list.deinit();

    list.addError(.unexpected_token, .{ .line = 1, .column = 1, .offset = 0 }, "error 1");
    list.enterPanicMode();
    list.addError(.unexpected_token, .{ .line = 1, .column = 3, .offset = 2 }, "error 2");
    list.addError(.unexpected_token, .{ .line = 1, .column = 5, .offset = 4 }, "error 3");
    list.exitPanicMode();
    list.addError(.unexpected_token, .{ .line = 1, .column = 7, .offset = 6 }, "error 4");

    // Only first and last should be recorded
    try std.testing.expectEqual(@as(usize, 2), list.errorCount());
}

test "max errors limit" {
    const source = "test";
    var list = ErrorList.init(std.testing.allocator, source);
    defer list.deinit();
    list.max_errors = 3;

    var i: usize = 0;
    while (i < 10) : (i += 1) {
        list.addError(.unexpected_token, .{ .line = 1, .column = 1, .offset = 0 }, "error");
    }

    try std.testing.expectEqual(@as(usize, 3), list.errorCount());
}
