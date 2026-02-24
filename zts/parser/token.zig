//! Token definitions for the JavaScript/JSX parser
//!
//! Enhanced tokens with full source location tracking for better error messages.

const std = @import("std");

/// Token types for JavaScript and JSX
pub const TokenType = enum(u8) {
    // Literals
    number,
    string_literal,
    identifier,
    true_lit,
    false_lit,
    null_lit,
    undefined_lit,
    regex_literal,

    // Template literals
    template_literal, // Complete `string`
    template_head, // `string${
    template_middle, // }string${
    template_tail, // }string`

    // Operators
    plus, // +
    minus, // -
    star, // *
    slash, // /
    percent, // %
    star_star, // **
    plus_plus, // ++
    minus_minus, // --

    // Comparison
    eq, // ==
    eq_eq, // ===
    ne, // !=
    ne_ne, // !==
    lt, // <
    le, // <=
    gt, // >
    ge, // >=

    // Logical & Bitwise
    ampersand, // &
    pipe, // |
    caret, // ^
    tilde, // ~
    ampersand_ampersand, // &&
    pipe_pipe, // ||
    question_question, // ??
    question_dot, // ?.
    bang, // !

    // Shift
    lt_lt, // <<
    gt_gt, // >>
    gt_gt_gt, // >>>

    // Assignment
    assign, // =
    plus_assign, // +=
    minus_assign, // -=
    star_assign, // *=
    slash_assign, // /=
    percent_assign, // %=
    ampersand_assign, // &=
    pipe_assign, // |=
    caret_assign, // ^=
    lt_lt_assign, // <<=
    gt_gt_assign, // >>=
    gt_gt_gt_assign, // >>>=
    star_star_assign, // **=
    ampersand_ampersand_assign, // &&=
    pipe_pipe_assign, // ||=
    question_question_assign, // ??=

    // Punctuation
    lparen, // (
    rparen, // )
    lbrace, // {
    rbrace, // }
    lbracket, // [
    rbracket, // ]
    comma, // ,
    dot, // .
    semicolon, // ;
    colon, // :
    question, // ?
    arrow, // =>
    spread, // ...

    // JSX-specific
    jsx_tag_open, // < in JSX context
    jsx_tag_close, // > in JSX context
    jsx_tag_self_close, // />
    jsx_tag_end_open, // </
    jsx_text, // Text content between JSX tags
    jsx_expr_start, // { in JSX context
    jsx_expr_end, // } in JSX context

    // Keywords
    kw_var,
    kw_let,
    kw_const,
    kw_function,
    kw_return,
    kw_if,
    kw_else,
    kw_while,
    kw_do,
    kw_for,
    kw_in,
    kw_of,
    kw_break,
    kw_continue,
    kw_switch,
    kw_case,
    kw_default,
    kw_throw,
    kw_try,
    kw_catch,
    kw_finally,
    kw_new,
    kw_this,
    kw_typeof,
    kw_void,
    kw_delete,
    kw_instanceof,
    kw_yield,
    kw_async,
    kw_await,
    kw_import,
    kw_export,
    kw_from,
    kw_as,
    kw_class,
    kw_extends,
    kw_super,
    kw_static,
    kw_get,
    kw_set,
    kw_debugger,
    kw_with,

    // TypeScript keywords (unsupported but tokenized for detection)
    kw_enum,
    kw_implements,
    kw_public,
    kw_private,
    kw_protected,

    // Decorator
    at_sign, // @

    // Special
    eof,
    newline, // Significant for ASI
    invalid,
};

/// Source location for error reporting
pub const SourceLocation = struct {
    line: u32,
    column: u16,
    offset: u32,

    pub fn format(self: SourceLocation, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{}:{}", .{ self.line, self.column });
    }
};

/// Token with full location information
pub const Token = struct {
    type: TokenType,
    start: u32, // Byte offset in source
    len: u16, // Token length in bytes
    line: u32, // 1-indexed line number
    column: u16, // 1-indexed column number

    /// Get the text of this token from the source
    pub fn text(self: Token, source: []const u8) []const u8 {
        const end = @min(self.start + self.len, source.len);
        return source[self.start..end];
    }

    /// Get a SourceLocation for this token
    pub fn location(self: Token) SourceLocation {
        return .{
            .line = self.line,
            .column = self.column,
            .offset = self.start,
        };
    }

    /// Create a "synthetic" token at a location (for error recovery)
    pub fn synthetic(token_type: TokenType, loc: SourceLocation) Token {
        return .{
            .type = token_type,
            .start = loc.offset,
            .len = 0,
            .line = loc.line,
            .column = loc.column,
        };
    }

    /// Check if this token is a keyword
    pub fn isKeyword(self: Token) bool {
        return @intFromEnum(self.type) >= @intFromEnum(TokenType.kw_var) and
            @intFromEnum(self.type) <= @intFromEnum(TokenType.kw_protected);
    }

    /// Check if this token is an assignment operator
    pub fn isAssignment(self: Token) bool {
        return switch (self.type) {
            .assign,
            .plus_assign,
            .minus_assign,
            .star_assign,
            .slash_assign,
            .percent_assign,
            .ampersand_assign,
            .pipe_assign,
            .caret_assign,
            .lt_lt_assign,
            .gt_gt_assign,
            .gt_gt_gt_assign,
            .star_star_assign,
            .ampersand_ampersand_assign,
            .pipe_pipe_assign,
            .question_question_assign,
            => true,
            else => false,
        };
    }

    /// Check if this token can start an expression
    pub fn canStartExpression(self: Token) bool {
        return switch (self.type) {
            .number,
            .string_literal,
            .identifier,
            .true_lit,
            .false_lit,
            .null_lit,
            .undefined_lit,
            .regex_literal,
            .template_literal,
            .template_head,
            .lparen,
            .lbracket,
            .lbrace,
            .plus,
            .minus,
            .bang,
            .tilde,
            .plus_plus,
            .minus_minus,
            .kw_new,
            .kw_this,
            .kw_typeof,
            .kw_void,
            .kw_delete,
            .kw_function,
            .kw_async,
            .kw_yield,
            .kw_await,
            .kw_class,
            .kw_super,
            .lt, // JSX element start
            => true,
            else => false,
        };
    }
};

/// Keyword lookup table
pub const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "var", .kw_var },
    .{ "let", .kw_let },
    .{ "const", .kw_const },
    .{ "function", .kw_function },
    .{ "return", .kw_return },
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "while", .kw_while },
    .{ "do", .kw_do },
    .{ "for", .kw_for },
    .{ "in", .kw_in },
    .{ "of", .kw_of },
    .{ "break", .kw_break },
    .{ "continue", .kw_continue },
    .{ "switch", .kw_switch },
    .{ "case", .kw_case },
    .{ "default", .kw_default },
    .{ "throw", .kw_throw },
    .{ "try", .kw_try },
    .{ "catch", .kw_catch },
    .{ "finally", .kw_finally },
    .{ "new", .kw_new },
    .{ "this", .kw_this },
    .{ "typeof", .kw_typeof },
    .{ "void", .kw_void },
    .{ "delete", .kw_delete },
    .{ "instanceof", .kw_instanceof },
    .{ "yield", .kw_yield },
    .{ "async", .kw_async },
    .{ "await", .kw_await },
    .{ "import", .kw_import },
    .{ "export", .kw_export },
    .{ "from", .kw_from },
    .{ "as", .kw_as },
    .{ "class", .kw_class },
    .{ "extends", .kw_extends },
    .{ "super", .kw_super },
    .{ "static", .kw_static },
    .{ "get", .kw_get },
    .{ "set", .kw_set },
    .{ "debugger", .kw_debugger },
    .{ "with", .kw_with },
    .{ "enum", .kw_enum },
    .{ "implements", .kw_implements },
    .{ "public", .kw_public },
    .{ "private", .kw_private },
    .{ "protected", .kw_protected },
    .{ "true", .true_lit },
    .{ "false", .false_lit },
    .{ "null", .null_lit },
    .{ "undefined", .undefined_lit },
});

/// Look up a keyword from an identifier string
pub fn lookupKeyword(ident: []const u8) ?TokenType {
    return keywords.get(ident);
}

test "token text extraction" {
    const source = "let x = 42;";
    const tok = Token{
        .type = .kw_let,
        .start = 0,
        .len = 3,
        .line = 1,
        .column = 1,
    };
    try std.testing.expectEqualStrings("let", tok.text(source));
}

test "keyword lookup" {
    try std.testing.expectEqual(TokenType.kw_function, lookupKeyword("function").?);
    try std.testing.expectEqual(TokenType.kw_const, lookupKeyword("const").?);
    try std.testing.expectEqual(TokenType.true_lit, lookupKeyword("true").?);
    try std.testing.expect(lookupKeyword("notakeyword") == null);
}

test "token location" {
    const tok = Token{
        .type = .identifier,
        .start = 10,
        .len = 5,
        .line = 2,
        .column = 3,
    };
    const loc = tok.location();
    try std.testing.expectEqual(@as(u32, 2), loc.line);
    try std.testing.expectEqual(@as(u16, 3), loc.column);
    try std.testing.expectEqual(@as(u32, 10), loc.offset);
}
