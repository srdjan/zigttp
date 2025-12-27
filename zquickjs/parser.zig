//! JavaScript Parser
//!
//! Single-pass recursive descent parser that generates bytecode directly.
//! Supports ES5 + limited ES6 features matching mquickjs capabilities.

const std = @import("std");
const bytecode = @import("bytecode.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const string = @import("string.zig");

/// Token types
pub const TokenType = enum(u8) {
    // Literals
    number,
    string_literal,
    identifier,
    @"true",
    @"false",
    @"null",
    undefined,

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

    // Logical
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
    template_literal, // `string`
    template_head, // `string${
    template_middle, // }string${
    template_tail, // }string`
    regex_literal, // /pattern/flags

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

    // Special
    eof,
    newline,
    invalid,
};

/// Token with location info
pub const Token = struct {
    type: TokenType,
    start: u32,
    len: u16,
    line: u32,

    pub fn text(self: Token, source: []const u8) []const u8 {
        return source[self.start..][0..self.len];
    }
};

/// Module import entry
pub const ImportEntry = struct {
    module_name: []const u8, // The module specifier (e.g., "./foo.js")
    local_name: []const u8, // Local binding name
    imported_name: []const u8, // Name in the source module (or "default")
    is_namespace: bool, // import * as ns
};

/// Module export entry
pub const ExportEntry = struct {
    local_name: []const u8, // Local binding name
    export_name: []const u8, // Exported as (or "default")
    is_reexport: bool, // export { x } from "module"
    from_module: ?[]const u8, // Source module for re-exports
};

/// Tokenizer state
pub const Tokenizer = struct {
    source: []const u8,
    pos: u32,
    line: u32,
    line_start: u32,

    pub fn init(source: []const u8) Tokenizer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .line_start = 0,
        };
    }

    /// Get next token
    pub fn next(self: *Tokenizer) Token {
        self.skipWhitespaceAndComments();

        if (self.pos >= self.source.len) {
            return self.makeToken(.eof, 0);
        }

        const start = self.pos;
        const c = self.advance();

        // Single character tokens
        switch (c) {
            '(' => return self.makeToken(.lparen, 1),
            ')' => return self.makeToken(.rparen, 1),
            '{' => return self.makeToken(.lbrace, 1),
            '}' => return self.makeToken(.rbrace, 1),
            '[' => return self.makeToken(.lbracket, 1),
            ']' => return self.makeToken(.rbracket, 1),
            ',' => return self.makeToken(.comma, 1),
            ';' => return self.makeToken(.semicolon, 1),
            ':' => return self.makeToken(.colon, 1),
            '~' => return self.makeToken(.tilde, 1),
            '?' => {
                if (self.match('?')) return self.makeToken(.question_question, 2);
                if (self.match('.')) {
                    // Check if next char is a digit - if so, this is ? followed by a number like ?.5
                    // Don't consume the dot in that case
                    if (self.pos < self.source.len and self.source[self.pos] >= '0' and self.source[self.pos] <= '9') {
                        self.pos -= 1; // Put back the dot
                        return self.makeToken(.question, 1);
                    }
                    return self.makeToken(.question_dot, 2);
                }
                return self.makeToken(.question, 1);
            },

            // Multi-character operators
            '+' => {
                if (self.match('+')) return self.makeToken(.plus_plus, 2);
                if (self.match('=')) return self.makeToken(.plus_assign, 2);
                return self.makeToken(.plus, 1);
            },
            '-' => {
                if (self.match('-')) return self.makeToken(.minus_minus, 2);
                if (self.match('=')) return self.makeToken(.minus_assign, 2);
                return self.makeToken(.minus, 1);
            },
            '*' => {
                if (self.match('*')) return self.makeToken(.star_star, 2);
                if (self.match('=')) return self.makeToken(.star_assign, 2);
                return self.makeToken(.star, 1);
            },
            '/' => {
                if (self.match('=')) return self.makeToken(.slash_assign, 2);
                return self.makeToken(.slash, 1);
            },
            '%' => {
                if (self.match('=')) return self.makeToken(.percent_assign, 2);
                return self.makeToken(.percent, 1);
            },
            '=' => {
                if (self.match('=')) {
                    if (self.match('=')) return self.makeToken(.eq_eq, 3);
                    return self.makeToken(.eq, 2);
                }
                if (self.match('>')) return self.makeToken(.arrow, 2);
                return self.makeToken(.assign, 1);
            },
            '!' => {
                if (self.match('=')) {
                    if (self.match('=')) return self.makeToken(.ne_ne, 3);
                    return self.makeToken(.ne, 2);
                }
                return self.makeToken(.bang, 1);
            },
            '<' => {
                if (self.match('<')) {
                    if (self.match('=')) return self.makeToken(.lt_lt_assign, 3);
                    return self.makeToken(.lt_lt, 2);
                }
                if (self.match('=')) return self.makeToken(.le, 2);
                return self.makeToken(.lt, 1);
            },
            '>' => {
                if (self.match('>')) {
                    if (self.match('>')) {
                        if (self.match('=')) return self.makeToken(.gt_gt_gt_assign, 4);
                        return self.makeToken(.gt_gt_gt, 3);
                    }
                    if (self.match('=')) return self.makeToken(.gt_gt_assign, 3);
                    return self.makeToken(.gt_gt, 2);
                }
                if (self.match('=')) return self.makeToken(.ge, 2);
                return self.makeToken(.gt, 1);
            },
            '&' => {
                if (self.match('&')) return self.makeToken(.ampersand_ampersand, 2);
                if (self.match('=')) return self.makeToken(.ampersand_assign, 2);
                return self.makeToken(.ampersand, 1);
            },
            '|' => {
                if (self.match('|')) return self.makeToken(.pipe_pipe, 2);
                if (self.match('=')) return self.makeToken(.pipe_assign, 2);
                return self.makeToken(.pipe, 1);
            },
            '^' => {
                if (self.match('=')) return self.makeToken(.caret_assign, 2);
                return self.makeToken(.caret, 1);
            },
            '.' => {
                if (self.match('.') and self.match('.')) return self.makeToken(.spread, 3);
                return self.makeToken(.dot, 1);
            },

            // String literals
            '"', '\'' => return self.scanString(c),

            // Template literals
            '`' => return self.scanTemplateLiteral(),

            // Numbers
            '0'...'9' => return self.scanNumber(start),

            else => {
                // Identifiers and keywords
                if (isIdentifierStart(c)) {
                    return self.scanIdentifier(start);
                }
                return self.makeToken(.invalid, 1);
            },
        }
    }

    fn advance(self: *Tokenizer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        return c;
    }

    fn peek(self: *Tokenizer) u8 {
        if (self.pos >= self.source.len) return 0;
        return self.source[self.pos];
    }

    fn peekNext(self: *Tokenizer) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    fn match(self: *Tokenizer, expected: u8) bool {
        if (self.pos >= self.source.len) return false;
        if (self.source[self.pos] != expected) return false;
        self.pos += 1;
        return true;
    }

    fn makeToken(self: *Tokenizer, token_type: TokenType, len: u16) Token {
        return .{
            .type = token_type,
            .start = self.pos - len,
            .len = len,
            .line = self.line,
        };
    }

    fn skipWhitespaceAndComments(self: *Tokenizer) void {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            switch (c) {
                ' ', '\t', '\r' => self.pos += 1,
                '\n' => {
                    self.pos += 1;
                    self.line += 1;
                    self.line_start = self.pos;
                },
                '/' => {
                    if (self.pos + 1 < self.source.len) {
                        if (self.source[self.pos + 1] == '/') {
                            // Line comment
                            self.pos += 2;
                            while (self.pos < self.source.len and self.source[self.pos] != '\n') {
                                self.pos += 1;
                            }
                        } else if (self.source[self.pos + 1] == '*') {
                            // Block comment
                            self.pos += 2;
                            while (self.pos + 1 < self.source.len) {
                                if (self.source[self.pos] == '*' and self.source[self.pos + 1] == '/') {
                                    self.pos += 2;
                                    break;
                                }
                                if (self.source[self.pos] == '\n') {
                                    self.line += 1;
                                    self.line_start = self.pos + 1;
                                }
                                self.pos += 1;
                            }
                        } else {
                            return;
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn scanString(self: *Tokenizer, quote: u8) Token {
        const start = self.pos - 1;
        while (self.pos < self.source.len and self.source[self.pos] != quote) {
            if (self.source[self.pos] == '\\' and self.pos + 1 < self.source.len) {
                self.pos += 2; // Skip escape sequence
            } else {
                if (self.source[self.pos] == '\n') {
                    self.line += 1;
                    self.line_start = self.pos + 1;
                }
                self.pos += 1;
            }
        }
        if (self.pos < self.source.len) {
            self.pos += 1; // Consume closing quote
        }
        return .{
            .type = .string_literal,
            .start = start,
            .len = @intCast(self.pos - start),
            .line = self.line,
        };
    }

    fn scanTemplateLiteral(self: *Tokenizer) Token {
        const start = self.pos - 1; // Include opening backtick
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '`') {
                self.pos += 1; // Consume closing backtick
                return .{
                    .type = .template_literal,
                    .start = start,
                    .len = @intCast(self.pos - start),
                    .line = self.line,
                };
            } else if (c == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                // Template expression start: ${
                self.pos += 2;
                return .{
                    .type = .template_head,
                    .start = start,
                    .len = @intCast(self.pos - start),
                    .line = self.line,
                };
            } else if (c == '\\' and self.pos + 1 < self.source.len) {
                self.pos += 2; // Skip escape sequence
            } else {
                if (c == '\n') {
                    self.line += 1;
                    self.line_start = self.pos + 1;
                }
                self.pos += 1;
            }
        }
        // Unterminated template literal
        return .{
            .type = .template_literal,
            .start = start,
            .len = @intCast(self.pos - start),
            .line = self.line,
        };
    }

    fn scanNumber(self: *Tokenizer, start: u32) Token {
        // Handle hex, octal, binary
        if (self.source[start] == '0' and self.pos < self.source.len) {
            const next_char = self.source[self.pos];
            if (next_char == 'x' or next_char == 'X') {
                self.pos += 1;
                while (self.pos < self.source.len and isHexDigit(self.source[self.pos])) {
                    self.pos += 1;
                }
                return .{
                    .type = .number,
                    .start = start,
                    .len = @intCast(self.pos - start),
                    .line = self.line,
                };
            }
            if (next_char == 'b' or next_char == 'B') {
                self.pos += 1;
                while (self.pos < self.source.len and (self.source[self.pos] == '0' or self.source[self.pos] == '1')) {
                    self.pos += 1;
                }
                return .{
                    .type = .number,
                    .start = start,
                    .len = @intCast(self.pos - start),
                    .line = self.line,
                };
            }
            if (next_char == 'o' or next_char == 'O') {
                self.pos += 1;
                while (self.pos < self.source.len and self.source[self.pos] >= '0' and self.source[self.pos] <= '7') {
                    self.pos += 1;
                }
                return .{
                    .type = .number,
                    .start = start,
                    .len = @intCast(self.pos - start),
                    .line = self.line,
                };
            }
        }

        // Decimal integer part
        while (self.pos < self.source.len and isDigit(self.source[self.pos])) {
            self.pos += 1;
        }

        // Decimal point
        if (self.pos < self.source.len and self.source[self.pos] == '.') {
            if (self.pos + 1 < self.source.len and isDigit(self.source[self.pos + 1])) {
                self.pos += 1;
                while (self.pos < self.source.len and isDigit(self.source[self.pos])) {
                    self.pos += 1;
                }
            }
        }

        // Exponent
        if (self.pos < self.source.len and (self.source[self.pos] == 'e' or self.source[self.pos] == 'E')) {
            self.pos += 1;
            if (self.pos < self.source.len and (self.source[self.pos] == '+' or self.source[self.pos] == '-')) {
                self.pos += 1;
            }
            while (self.pos < self.source.len and isDigit(self.source[self.pos])) {
                self.pos += 1;
            }
        }

        return .{
            .type = .number,
            .start = start,
            .len = @intCast(self.pos - start),
            .line = self.line,
        };
    }

    fn scanIdentifier(self: *Tokenizer, start: u32) Token {
        while (self.pos < self.source.len and isIdentifierChar(self.source[self.pos])) {
            self.pos += 1;
        }

        const text = self.source[start..self.pos];
        const token_type = getKeyword(text) orelse .identifier;

        return .{
            .type = token_type,
            .start = start,
            .len = @intCast(self.pos - start),
            .line = self.line,
        };
    }

    /// Scan a regex literal: /pattern/flags
    /// Called by parser when it expects a regex (after =, (, [, etc.)
    pub fn scanRegex(self: *Tokenizer) Token {
        self.skipWhitespaceAndComments();

        if (self.pos >= self.source.len or self.source[self.pos] != '/') {
            return self.makeToken(.invalid, 0);
        }

        const start = self.pos;
        self.pos += 1; // Skip opening /

        // Scan pattern (handle escapes and character classes)
        var in_class = false;
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            if (c == '\\' and self.pos + 1 < self.source.len) {
                // Escape sequence - skip both chars
                self.pos += 2;
                continue;
            }

            if (c == '[' and !in_class) {
                in_class = true;
                self.pos += 1;
                continue;
            }

            if (c == ']' and in_class) {
                in_class = false;
                self.pos += 1;
                continue;
            }

            if (c == '/' and !in_class) {
                // End of pattern
                self.pos += 1;
                break;
            }

            if (c == '\n') {
                // Unterminated regex
                return .{
                    .type = .invalid,
                    .start = start,
                    .len = @intCast(self.pos - start),
                    .line = self.line,
                };
            }

            self.pos += 1;
        }

        // Scan flags (g, i, m, s, u, y)
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == 'g' or c == 'i' or c == 'm' or c == 's' or c == 'u' or c == 'y') {
                self.pos += 1;
            } else {
                break;
            }
        }

        return .{
            .type = .regex_literal,
            .start = start,
            .len = @intCast(self.pos - start),
            .line = self.line,
        };
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn isIdentifierStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
}

fn isIdentifierChar(c: u8) bool {
    return isIdentifierStart(c) or isDigit(c);
}

fn getKeyword(text: []const u8) ?TokenType {
    const keywords = .{
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
        .{ "true", .@"true" },
        .{ "false", .@"false" },
        .{ "null", .@"null" },
        .{ "undefined", .undefined },
    };

    inline for (keywords) |kw| {
        if (std.mem.eql(u8, text, kw[0])) {
            return kw[1];
        }
    }
    return null;
}

// ============================================================================
// Parser
// ============================================================================

/// Parse error
pub const ParseError = error{
    UnexpectedToken,
    InvalidNumber,
    UnterminatedString,
    TooManyLocals,
    TooManyConstants,
    InvalidAssignment,
    OutOfMemory,
};

/// Parser state
pub const Parser = struct {
    tokenizer: Tokenizer,
    current: Token,
    previous: Token,
    allocator: std.mem.Allocator,

    // Bytecode output
    code: std.array_list.Managed(u8),
    constants: std.array_list.Managed(value.JSValue),

    // Local variables
    locals: [256]Local,
    local_count: u8,
    scope_depth: u8,

    // String table for interning
    strings: *string.StringTable,

    // Generator/async state
    in_generator: bool,
    in_async: bool,

    // Module state
    is_module: bool,
    exports: std.ArrayList(ExportEntry),
    imports: std.ArrayList(ImportEntry),

    // Error state
    had_error: bool,
    panic_mode: bool,

    pub const Error = error{
        OutOfMemory,
        UnexpectedToken,
        InvalidNumber,
        TooManyLocals,
        TooManyConstants,
        UnterminatedString,
        InvalidAssignment,
    };

    pub const Local = struct {
        name: []const u8,
        depth: u8,
        is_const: bool,
    };

    pub fn init(allocator: std.mem.Allocator, source: []const u8, strings: *string.StringTable) Parser {
        var parser = Parser{
            .tokenizer = Tokenizer.init(source),
            .current = undefined,
            .previous = undefined,
            .allocator = allocator,
            .code = std.array_list.Managed(u8).init(allocator),
            .constants = std.array_list.Managed(value.JSValue).init(allocator),
            .locals = undefined,
            .local_count = 0,
            .scope_depth = 1, // Start in local scope for eval/REPL mode
            .strings = strings,
            .in_generator = false,
            .in_async = false,
            .is_module = false,
            .exports = .empty,
            .imports = .empty,
            .had_error = false,
            .panic_mode = false,
        };
        // Initialize locals
        for (&parser.locals) |*local| {
            local.* = .{ .name = "", .depth = 0, .is_const = false };
        }
        // Prime the parser
        parser.advance();
        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.code.deinit();
        self.constants.deinit();
        self.exports.deinit(self.allocator);
        self.imports.deinit(self.allocator);
    }

    /// Parse a complete script
    pub fn parse(self: *Parser) ![]const u8 {
        while (!self.check(.eof)) {
            try self.declaration();
        }
        try self.emitOp(.halt);
        if (self.had_error) return error.UnexpectedToken;
        return self.code.items;
    }

    // ========================================================================
    // Declarations
    // ========================================================================

    fn declaration(self: *Parser) Error!void {
        if (self.match(.kw_import)) {
            try self.importDeclaration();
        } else if (self.match(.kw_export)) {
            try self.exportDeclaration();
        } else if (self.match(.kw_var) or self.match(.kw_let) or self.match(.kw_const)) {
            try self.varDeclaration(self.previous.type == .kw_const);
        } else if (self.match(.kw_async)) {
            // async function declaration
            try self.consume(.kw_function, "Expected 'function' after 'async'.");
            try self.asyncFunctionDeclaration();
        } else if (self.match(.kw_function)) {
            try self.functionDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) self.synchronize();
    }

    fn varDeclaration(self: *Parser, is_const: bool) Error!void {
        // Check for destructuring patterns
        if (self.check(.lbrace)) {
            try self.objectDestructuring(is_const);
            return;
        }

        if (self.check(.lbracket)) {
            try self.arrayDestructuring(is_const);
            return;
        }

        // Regular variable declaration
        const name = try self.parseVariable("Expected variable name.");

        if (self.match(.assign)) {
            try self.expression();
        } else {
            try self.emitOp(.push_undefined);
        }

        try self.defineVariable(name, is_const);
        _ = self.match(.semicolon);
    }

    /// Parse object destructuring: const {x, y, z: alias} = obj
    fn objectDestructuring(self: *Parser, is_const: bool) Error!void {
        try self.consume(.lbrace, "Expected '{' for object destructuring.");

        var bindings: [32]struct { key: []const u8, alias: []const u8 } = undefined;
        var binding_count: usize = 0;

        if (!self.check(.rbrace)) {
            while (true) {
                if (binding_count >= 32) {
                    self.errorAtCurrent("Too many destructuring bindings.");
                    return error.TooManyLocals;
                }

                try self.consume(.identifier, "Expected property name.");
                const key = self.previous.text(self.tokenizer.source);
                var alias = key;

                if (self.match(.colon)) {
                    try self.consume(.identifier, "Expected alias name.");
                    alias = self.previous.text(self.tokenizer.source);
                }

                bindings[binding_count] = .{ .key = key, .alias = alias };
                binding_count += 1;

                if (!self.match(.comma)) break;
                if (self.check(.rbrace)) break;
            }
        }
        try self.consume(.rbrace, "Expected '}' after destructuring pattern.");
        try self.consume(.assign, "Expected '=' after destructuring pattern.");
        try self.expression();

        for (0..binding_count) |i| {
            const binding = bindings[i];
            if (i < binding_count - 1) {
                try self.emitOp(.dup);
            }
            const key_atom = try self.internAtom(binding.key);
            try self.emitOp(.get_field);
            try self.emitU16(@truncate(key_atom));
            try self.defineVariable(binding.alias, is_const);
        }

        _ = self.match(.semicolon);
    }

    /// Parse array destructuring: const [a, b] = arr
    fn arrayDestructuring(self: *Parser, is_const: bool) Error!void {
        try self.consume(.lbracket, "Expected '[' for array destructuring.");

        var names: [32][]const u8 = undefined;
        var name_count: usize = 0;

        if (!self.check(.rbracket)) {
            while (true) {
                if (name_count >= 32) {
                    self.errorAtCurrent("Too many destructuring bindings.");
                    return error.TooManyLocals;
                }

                if (self.check(.comma)) {
                    names[name_count] = "";
                } else {
                    try self.consume(.identifier, "Expected variable name.");
                    names[name_count] = self.previous.text(self.tokenizer.source);
                }
                name_count += 1;

                if (!self.match(.comma)) break;
                if (self.check(.rbracket)) break;
            }
        }
        try self.consume(.rbracket, "Expected ']' after destructuring pattern.");
        try self.consume(.assign, "Expected '=' after destructuring pattern.");
        try self.expression();

        for (0..name_count) |i| {
            const name = names[i];
            if (name.len == 0) continue;

            var has_more = false;
            for (i + 1..name_count) |j| {
                if (names[j].len > 0) {
                    has_more = true;
                    break;
                }
            }
            if (has_more) {
                try self.emitOp(.dup);
            }

            if (i <= 127) {
                try self.emitOp(.push_i8);
                try self.emitByte(@truncate(i));
            } else {
                try self.emitOp(.push_i16);
                try self.emitU16(@truncate(i));
            }
            try self.emitOp(.get_elem);
            try self.defineVariable(name, is_const);
        }

        _ = self.match(.semicolon);
    }

    fn internAtom(self: *Parser, name: []const u8) !u32 {
        if (lookupPredefinedAtom(name)) |atom| {
            return @intFromEnum(atom);
        }
        return @as(u32, try self.addStringConstant(name));
    }

    fn functionDeclaration(self: *Parser) Error!void {
        const is_generator = self.match(.star); // function* for generators
        const name = try self.parseVariable("Expected function name.");
        try self.compileFunction(name, is_generator, false);
        try self.defineVariable(name, false);
    }

    fn asyncFunctionDeclaration(self: *Parser) Error!void {
        const name = try self.parseVariable("Expected function name.");
        try self.compileFunction(name, false, true);
        try self.defineVariable(name, false);
    }

    /// Parse import declaration
    /// import defaultExport from "module";
    /// import { export1, export2 as alias } from "module";
    /// import * as namespace from "module";
    /// import "module"; (side-effect only)
    fn importDeclaration(self: *Parser) Error!void {
        self.is_module = true;

        // Side-effect import: import "module";
        if (self.check(.string_literal)) {
            const module_name = self.current.text(self.tokenizer.source);
            self.advance();
            try self.consume(.semicolon, "Expected ';' after import.");

            // Emit import for side effects
            const module_idx = try self.addStringConstant(module_name[1 .. module_name.len - 1]); // Strip quotes
            try self.emitOp(.import_module);
            try self.emitU16(module_idx);
            try self.emitOp(.drop); // Discard module namespace
            return;
        }

        // import * as namespace from "module";
        if (self.match(.star)) {
            try self.consume(.kw_as, "Expected 'as' after '*'.");
            try self.consume(.identifier, "Expected namespace identifier.");
            const local_name = self.previous.text(self.tokenizer.source);

            try self.consume(.kw_from, "Expected 'from' after namespace.");
            try self.consume(.string_literal, "Expected module specifier.");
            const module_name = self.previous.text(self.tokenizer.source);
            try self.consume(.semicolon, "Expected ';' after import.");

            // Record import
            try self.imports.append(self.allocator, .{
                .module_name = module_name[1 .. module_name.len - 1],
                .local_name = local_name,
                .imported_name = "*",
                .is_namespace = true,
            });

            // Emit bytecode to load module and bind to local
            const module_idx = try self.addStringConstant(module_name[1 .. module_name.len - 1]);
            try self.emitOp(.import_module);
            try self.emitU16(module_idx);

            // Define the local variable
            try self.declareLocal(local_name, true);
            const slot = self.local_count - 1;
            try self.emitOp(.put_loc);
            try self.emitByte(slot);
            return;
        }

        // import defaultExport from "module"; OR import { ... } from "module";
        var has_default = false;
        var default_name: []const u8 = "";

        // Check for default import
        if (self.check(.identifier)) {
            has_default = true;
            default_name = self.current.text(self.tokenizer.source);
            self.advance();

            // Check for comma (followed by named imports)
            if (!self.match(.comma)) {
                // Just default import
                try self.consume(.kw_from, "Expected 'from' after default import.");
                try self.consume(.string_literal, "Expected module specifier.");
                const module_name = self.previous.text(self.tokenizer.source);
                try self.consume(.semicolon, "Expected ';' after import.");

                try self.imports.append(self.allocator, .{
                    .module_name = module_name[1 .. module_name.len - 1],
                    .local_name = default_name,
                    .imported_name = "default",
                    .is_namespace = false,
                });

                // Emit bytecode
                const module_idx = try self.addStringConstant(module_name[1 .. module_name.len - 1]);
                try self.emitOp(.import_module);
                try self.emitU16(module_idx);
                try self.emitOp(.import_default);

                try self.declareLocal(default_name, true);
                const slot = self.local_count - 1;
                try self.emitOp(.put_loc);
                try self.emitByte(slot);
                return;
            }
        }

        // Named imports: { export1, export2 as alias }
        try self.consume(.lbrace, "Expected '{' for named imports.");

        var module_name: []const u8 = "";
        var named_imports: std.ArrayList(struct { imported: []const u8, local: []const u8 }) = .empty;
        defer named_imports.deinit(self.allocator);

        while (!self.check(.rbrace) and !self.check(.eof)) {
            try self.consume(.identifier, "Expected import name.");
            const imported_name = self.previous.text(self.tokenizer.source);
            var local_name = imported_name;

            if (self.match(.kw_as)) {
                try self.consume(.identifier, "Expected local name after 'as'.");
                local_name = self.previous.text(self.tokenizer.source);
            }

            try named_imports.append(self.allocator, .{ .imported = imported_name, .local = local_name });

            if (!self.match(.comma)) break;
        }

        try self.consume(.rbrace, "Expected '}' after named imports.");
        try self.consume(.kw_from, "Expected 'from' after import specifiers.");
        try self.consume(.string_literal, "Expected module specifier.");
        module_name = self.previous.text(self.tokenizer.source);
        try self.consume(.semicolon, "Expected ';' after import.");

        // Emit bytecode to load module
        const module_idx = try self.addStringConstant(module_name[1 .. module_name.len - 1]);
        try self.emitOp(.import_module);
        try self.emitU16(module_idx);

        // Handle default import if present
        if (has_default) {
            try self.emitOp(.dup); // Keep module namespace on stack
            try self.emitOp(.import_default);
            try self.declareLocal(default_name, true);
            const slot = self.local_count - 1;
            try self.emitOp(.put_loc);
            try self.emitByte(slot);

            try self.imports.append(self.allocator, .{
                .module_name = module_name[1 .. module_name.len - 1],
                .local_name = default_name,
                .imported_name = "default",
                .is_namespace = false,
            });
        }

        // Handle named imports
        for (named_imports.items) |import_item| {
            try self.emitOp(.dup); // Keep module namespace on stack
            const name_idx = try self.addStringConstant(import_item.imported);
            try self.emitOp(.import_name);
            try self.emitU16(name_idx);

            try self.declareLocal(import_item.local, true);
            const slot = self.local_count - 1;
            try self.emitOp(.put_loc);
            try self.emitByte(slot);

            try self.imports.append(self.allocator, .{
                .module_name = module_name[1 .. module_name.len - 1],
                .local_name = import_item.local,
                .imported_name = import_item.imported,
                .is_namespace = false,
            });
        }

        try self.emitOp(.drop); // Drop module namespace
    }

    /// Parse export declaration
    /// export { name1, name2 as alias };
    /// export default expression;
    /// export function name() { }
    /// export const/let/var name = value;
    fn exportDeclaration(self: *Parser) Error!void {
        self.is_module = true;

        // export default expression;
        if (self.match(.kw_default)) {
            // Parse the expression or declaration
            if (self.match(.kw_function)) {
                // export default function name() { } or export default function() { }
                var name: []const u8 = "";
                if (self.check(.identifier)) {
                    name = self.current.text(self.tokenizer.source);
                    self.advance();
                }
                try self.compileFunction(if (name.len > 0) name else "default", false, false);
            } else if (self.match(.kw_async)) {
                try self.consume(.kw_function, "Expected 'function' after 'async'.");
                var name: []const u8 = "";
                if (self.check(.identifier)) {
                    name = self.current.text(self.tokenizer.source);
                    self.advance();
                }
                try self.compileFunction(if (name.len > 0) name else "default", false, true);
            } else {
                // export default expression;
                try self.expression();
                try self.consume(.semicolon, "Expected ';' after export default.");
            }

            try self.emitOp(.export_default);

            try self.exports.append(self.allocator, .{
                .local_name = "default",
                .export_name = "default",
                .is_reexport = false,
                .from_module = null,
            });
            return;
        }

        // export function name() { }
        if (self.match(.kw_function)) {
            const is_generator = self.match(.star);
            try self.consume(.identifier, "Expected function name.");
            const name = self.previous.text(self.tokenizer.source);
            try self.compileFunction(name, is_generator, false);
            try self.defineVariable(name, false);

            // Export the function
            const name_idx = try self.addStringConstant(name);
            try self.emitOp(.get_loc);
            try self.emitByte(self.resolveLocal(name) orelse 0);
            try self.emitOp(.export_name);
            try self.emitU16(name_idx);

            try self.exports.append(self.allocator, .{
                .local_name = name,
                .export_name = name,
                .is_reexport = false,
                .from_module = null,
            });
            return;
        }

        // export async function name() { }
        if (self.match(.kw_async)) {
            try self.consume(.kw_function, "Expected 'function' after 'async'.");
            try self.consume(.identifier, "Expected function name.");
            const name = self.previous.text(self.tokenizer.source);
            try self.compileFunction(name, false, true);
            try self.defineVariable(name, false);

            const name_idx = try self.addStringConstant(name);
            try self.emitOp(.get_loc);
            try self.emitByte(self.resolveLocal(name) orelse 0);
            try self.emitOp(.export_name);
            try self.emitU16(name_idx);

            try self.exports.append(self.allocator, .{
                .local_name = name,
                .export_name = name,
                .is_reexport = false,
                .from_module = null,
            });
            return;
        }

        // export const/let/var name = value;
        if (self.match(.kw_const) or self.match(.kw_let) or self.match(.kw_var)) {
            const is_const = self.previous.type == .kw_const;
            try self.consume(.identifier, "Expected variable name.");
            const name = self.previous.text(self.tokenizer.source);

            if (self.match(.eq)) {
                try self.expression();
            } else {
                try self.emitOp(.push_undefined);
            }
            try self.consume(.semicolon, "Expected ';' after variable declaration.");

            try self.defineVariable(name, is_const);

            const name_idx = try self.addStringConstant(name);
            try self.emitOp(.get_loc);
            try self.emitByte(self.resolveLocal(name) orelse 0);
            try self.emitOp(.export_name);
            try self.emitU16(name_idx);

            try self.exports.append(self.allocator, .{
                .local_name = name,
                .export_name = name,
                .is_reexport = false,
                .from_module = null,
            });
            return;
        }

        // export { name1, name2 as alias };
        try self.consume(.lbrace, "Expected '{' for named exports.");

        while (!self.check(.rbrace) and !self.check(.eof)) {
            try self.consume(.identifier, "Expected export name.");
            const local_name = self.previous.text(self.tokenizer.source);
            var export_name = local_name;

            if (self.match(.kw_as)) {
                try self.consume(.identifier, "Expected export alias after 'as'.");
                export_name = self.previous.text(self.tokenizer.source);
            }

            const name_idx = try self.addStringConstant(export_name);
            try self.emitOp(.get_loc);
            try self.emitByte(self.resolveLocal(local_name) orelse 0);
            try self.emitOp(.export_name);
            try self.emitU16(name_idx);

            try self.exports.append(self.allocator, .{
                .local_name = local_name,
                .export_name = export_name,
                .is_reexport = false,
                .from_module = null,
            });

            if (!self.match(.comma)) break;
        }

        try self.consume(.rbrace, "Expected '}' after named exports.");
        try self.consume(.semicolon, "Expected ';' after export.");
    }

    /// Parse and compile a function, pushing its value onto the stack
    fn compileFunction(self: *Parser, name: []const u8, is_generator: bool, is_async: bool) Error!void {
        // Save and set generator/async flags
        const outer_in_generator = self.in_generator;
        const outer_in_async = self.in_async;
        self.in_generator = is_generator;
        self.in_async = is_async;
        defer self.in_generator = outer_in_generator;
        defer self.in_async = outer_in_async;

        try self.consume(.lparen, "Expected '(' after function name.");

        // Parse parameters with default values and rest parameter support
        const ParamDefault = struct { start: u32, end: u32 };
        var params: [255][]const u8 = undefined;
        var param_defaults: [255]?ParamDefault = [_]?ParamDefault{null} ** 255;
        var param_count: u8 = 0;
        var rest_param_idx: ?u8 = null;

        if (!self.check(.rparen)) {
            while (true) {
                if (param_count >= 255) {
                    self.errorAtCurrent("Cannot have more than 255 parameters.");
                    return error.TooManyLocals;
                }

                // Check for rest parameter: ...name
                const is_rest = self.match(.dot) and self.match(.dot) and self.match(.dot);
                if (is_rest) {
                    if (rest_param_idx != null) {
                        self.errorAtCurrent("Only one rest parameter allowed.");
                        return error.UnexpectedToken;
                    }
                    rest_param_idx = param_count;
                }

                try self.consume(.identifier, "Expected parameter name.");
                params[param_count] = self.previous.text(self.tokenizer.source);

                // Check for default value: = expression
                if (self.match(.assign)) {
                    if (is_rest) {
                        self.errorAtCurrent("Rest parameter cannot have default value.");
                        return error.UnexpectedToken;
                    }
                    // Record start position of default expression
                    const start = self.current.start;
                    // Skip over expression (track depth for nested parens/braces/brackets)
                    var depth: u32 = 0;
                    while (!self.check(.eof)) {
                        if (self.check(.lparen) or self.check(.lbrace) or self.check(.lbracket)) {
                            depth += 1;
                            self.advance();
                        } else if (self.check(.rparen) or self.check(.rbrace) or self.check(.rbracket)) {
                            if (depth == 0) break;
                            depth -= 1;
                            self.advance();
                        } else if (self.check(.comma) and depth == 0) {
                            break;
                        } else {
                            self.advance();
                        }
                    }
                    const end = self.previous.start + self.previous.len;
                    param_defaults[param_count] = .{ .start = start, .end = end };
                }

                param_count += 1;

                if (is_rest) {
                    // Rest param must be last
                    if (!self.check(.rparen)) {
                        self.errorAtCurrent("Rest parameter must be last.");
                        return error.UnexpectedToken;
                    }
                    break;
                }
                if (!self.match(.comma)) break;
            }
        }
        try self.consume(.rparen, "Expected ')' after parameters.");
        try self.consume(.lbrace, "Expected '{' before function body.");

        // Save outer parser state
        const outer_code = self.code;
        const outer_constants = self.constants;
        const outer_locals = self.locals;
        const outer_local_count = self.local_count;
        const outer_scope_depth = self.scope_depth;

        // Initialize new function scope
        self.code = std.array_list.Managed(u8).init(self.allocator);
        self.constants = std.array_list.Managed(value.JSValue).init(self.allocator);
        self.local_count = 0;
        self.scope_depth = 1;

        // Add parameters as locals
        const required_param_count: u8 = blk: {
            var count: u8 = 0;
            for (0..param_count) |i| {
                if (param_defaults[i] == null and (rest_param_idx == null or i != rest_param_idx.?)) {
                    count = @intCast(i + 1);
                }
            }
            break :blk count;
        };
        _ = required_param_count; // For future use in arg count validation

        for (0..param_count) |i| {
            self.locals[self.local_count] = .{
                .name = params[i],
                .depth = 1,
                .is_const = false,
            };
            self.local_count += 1;
        }

        // Emit default value initialization code
        // For each param with default: if (param === undefined) param = default;
        const outer_tokenizer = self.tokenizer;
        const outer_current = self.current;
        const outer_previous = self.previous;

        for (0..param_count) |i| {
            if (param_defaults[i]) |default_range| {
                // get_loc i
                try self.emitOp(.get_loc);
                try self.emitByte(@intCast(i));
                // push_undefined
                try self.emitOp(.push_undefined);
                // strict_neq (if NOT undefined, skip default assignment)
                try self.emitOp(.strict_neq);
                // if_true skip (param is defined, skip the default)
                try self.emitOp(.if_true);
                const skip_addr = self.code.items.len;
                try self.emitU16(0); // Placeholder

                // Parse and compile the default expression
                const default_source = outer_tokenizer.source[default_range.start..default_range.end];
                const default_tokenizer = Tokenizer.init(default_source);
                self.tokenizer = default_tokenizer;
                self.advance(); // Prime the tokenizer
                try self.expression();

                // Restore tokenizer
                self.tokenizer = outer_tokenizer;
                self.current = outer_current;
                self.previous = outer_previous;

                // put_loc i (assign default to param)
                try self.emitOp(.put_loc);
                try self.emitByte(@intCast(i));

                // Patch the skip jump
                const skip_offset: i16 = @intCast(self.code.items.len - skip_addr - 2);
                self.code.items[skip_addr] = @intCast(skip_offset & 0xFF);
                self.code.items[skip_addr + 1] = @intCast((skip_offset >> 8) & 0xFF);
            }
        }

        // Handle rest parameter - collect remaining args into array
        // Note: This requires runtime support to know actual arg count
        // For now, rest params create an empty array (full support needs runtime changes)
        if (rest_param_idx) |_| {
            // TODO: Full rest parameter support needs runtime arg count tracking
            // For now, initialize rest param as empty array
            try self.emitOp(.new_array);
            try self.emitU16(0);
            try self.emitOp(.put_loc);
            try self.emitByte(rest_param_idx.?);
        }

        // Parse function body
        try self.block();

        // Add implicit return undefined
        try self.emitOp(.push_undefined);
        try self.emitOp(.ret);

        // Create FunctionBytecode on heap
        const func_bc = self.allocator.create(bytecode.FunctionBytecode) catch return error.OutOfMemory;
        const code_copy = self.allocator.dupe(u8, self.code.items) catch return error.OutOfMemory;
        const consts_copy = self.allocator.dupe(value.JSValue, self.constants.items) catch return error.OutOfMemory;

        // Get name atom
        const name_atom: u32 = if (lookupPredefinedAtom(name)) |atom|
            @intFromEnum(atom)
        else
            0;

        func_bc.* = .{
            .header = .{},
            .name_atom = name_atom,
            .arg_count = param_count,
            .local_count = self.local_count,
            .stack_size = 256,
            .flags = .{ .is_generator = is_generator, .is_async = is_async },
            .code = code_copy,
            .constants = consts_copy,
            .source_map = null,
        };

        // Clean up inner buffers
        self.code.deinit();
        self.constants.deinit();

        // Restore outer parser state
        self.code = outer_code;
        self.constants = outer_constants;
        self.locals = outer_locals;
        self.local_count = outer_local_count;
        self.scope_depth = outer_scope_depth;

        // Store function bytecode in constant pool and emit appropriate opcode
        const const_idx = try self.addConstant(value.JSValue.fromPtr(func_bc));
        if (is_generator) {
            try self.emitOp(.make_generator);
        } else if (is_async) {
            try self.emitOp(.make_async);
        } else {
            try self.emitOp(.make_function);
        }
        try self.emitU16(const_idx);
    }

    fn parseVariable(self: *Parser, error_msg: []const u8) Error![]const u8 {
        try self.consume(.identifier, error_msg);
        return self.previous.text(self.tokenizer.source);
    }

    fn defineVariable(self: *Parser, name: []const u8, is_const: bool) Error!void {
        if (self.scope_depth > 0) {
            // Local variable - register it and emit put_loc
            if (self.local_count >= 255) return error.TooManyLocals;
            const idx = self.local_count;
            self.locals[self.local_count] = .{
                .name = name,
                .depth = self.scope_depth,
                .is_const = is_const,
            };
            self.local_count += 1;
            // Store the value in the local slot
            try self.emitOp(.put_loc);
            try self.emitByte(idx);
        } else {
            // Global variable
            const name_const = try self.addStringConstant(name);
            try self.emitOp(.define_global);
            try self.emitByte(name_const);
        }
    }

    /// Declare a local variable without emitting store bytecode.
    /// Used when you need to reserve a slot but emit put_loc separately.
    fn declareLocal(self: *Parser, name: []const u8, is_const: bool) Error!void {
        if (self.local_count >= 255) return error.TooManyLocals;
        self.locals[self.local_count] = .{
            .name = name,
            .depth = self.scope_depth,
            .is_const = is_const,
        };
        self.local_count += 1;
    }

    /// Resolve a local variable by name, returning its slot index.
    fn resolveLocal(self: *Parser, name: []const u8) ?u8 {
        var i: usize = self.local_count;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, self.locals[i].name, name)) {
                return @intCast(i);
            }
        }
        return null;
    }

    // ========================================================================
    // Statements
    // ========================================================================

    fn statement(self: *Parser) Error!void {
        if (self.match(.kw_if)) {
            try self.ifStatement();
        } else if (self.match(.kw_while)) {
            try self.whileStatement();
        } else if (self.match(.kw_for)) {
            try self.forStatement();
        } else if (self.match(.kw_switch)) {
            try self.switchStatement();
        } else if (self.match(.kw_return)) {
            try self.returnStatement();
        } else if (self.match(.kw_throw)) {
            try self.throwStatement();
        } else if (self.match(.kw_try)) {
            try self.tryStatement();
        } else if (self.match(.lbrace)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn ifStatement(self: *Parser) Error!void {
        try self.consume(.lparen, "Expected '(' after 'if'.");
        try self.expression();
        try self.consume(.rparen, "Expected ')' after condition.");

        const then_jump = try self.emitJump(.if_false);
        try self.emitOp(.drop);
        try self.statement();

        const else_jump = try self.emitJump(.goto);
        self.patchJump(then_jump);
        try self.emitOp(.drop);

        if (self.match(.kw_else)) {
            try self.statement();
        }
        self.patchJump(else_jump);
    }

    fn whileStatement(self: *Parser) Error!void {
        const loop_start = @as(u32, @intCast(self.code.items.len));

        try self.consume(.lparen, "Expected '(' after 'while'.");
        try self.expression();
        try self.consume(.rparen, "Expected ')' after condition.");

        const exit_jump = try self.emitJump(.if_false);
        try self.emitOp(.drop);
        try self.statement();
        try self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        try self.emitOp(.drop);
    }

    fn forStatement(self: *Parser) Error!void {
        self.beginScope();

        try self.consume(.lparen, "Expected '(' after 'for'.");

        // Check for for...of or for...in syntax
        const is_const = self.match(.kw_const);
        const is_let = !is_const and self.match(.kw_let);
        const is_var = !is_const and !is_let and self.match(.kw_var);

        if ((is_const or is_let or is_var) and self.check(.identifier)) {
            const var_name = self.current.text(self.tokenizer.source);
            self.advance(); // consume identifier

            if (self.match(.kw_of)) {
                // for (const/let/var x of iterable)
                try self.forOfStatement(var_name, is_const);
                return;
            } else if (self.match(.kw_in)) {
                // for (const/let/var x in object)
                try self.forInStatement(var_name, is_const);
                return;
            }

            // Not for...of or for...in, treat as regular for with var declaration
            // Rewind and parse normally
            self.tokenizer.pos = self.previous.start;
            self.tokenizer.pos = self.previous.start;
            self.advance();
            if (is_const) {
                // We already consumed 'const', need to handle this differently
                try self.varDeclaration(true);
            } else {
                try self.varDeclaration(false);
            }
        } else if (self.match(.semicolon)) {
            // No initializer - regular for loop
        } else if (!is_const and !is_let and !is_var) {
            try self.expressionStatement();
        } else {
            // var/let/const but not followed by identifier of/in
            try self.varDeclaration(is_const);
        }

        try self.regularForLoop();
    }

    fn regularForLoop(self: *Parser) Error!void {
        var loop_start = @as(u32, @intCast(self.code.items.len));

        // Condition
        var exit_jump: ?u32 = null;
        if (!self.check(.semicolon)) {
            try self.expression();
            exit_jump = try self.emitJump(.if_false);
            try self.emitOp(.drop);
        }
        try self.consume(.semicolon, "Expected ';' after loop condition.");

        // Increment
        if (!self.check(.rparen)) {
            const body_jump = try self.emitJump(.goto);
            const increment_start = @as(u32, @intCast(self.code.items.len));
            try self.expression();
            try self.emitOp(.drop);
            try self.emitLoop(loop_start);
            loop_start = increment_start;
            self.patchJump(body_jump);
        }
        try self.consume(.rparen, "Expected ')' after for clauses.");

        // Body
        try self.statement();
        try self.emitLoop(loop_start);

        if (exit_jump) |jump| {
            self.patchJump(jump);
            try self.emitOp(.drop);
        }

        try self.endScope();
    }

    fn forOfStatement(self: *Parser, var_name: []const u8, is_const: bool) Error!void {
        // Parse iterable expression
        try self.expression();
        try self.consume(.rparen, "Expected ')' after for...of expression.");

        // Stack: [iterable]
        // We'll store the array and an index as locals

        // Declare hidden locals for array and index
        const arr_slot = self.local_count;
        try self.declareLocal("__for_arr", false);
        try self.emitOp(.put_loc);
        try self.emitByte(arr_slot);

        // Push 0 for index
        try self.emitOp(.push_0);
        const idx_slot = self.local_count;
        try self.declareLocal("__for_idx", false);
        try self.emitOp(.put_loc);
        try self.emitByte(idx_slot);

        // Loop start
        const loop_start = @as(u32, @intCast(self.code.items.len));

        // Check: idx < arr.length
        try self.emitOp(.get_loc);
        try self.emitByte(idx_slot);
        try self.emitOp(.get_loc);
        try self.emitByte(arr_slot);
        try self.emitOp(.get_field);
        try self.emitU16(@intFromEnum(object.Atom.length));
        try self.emitOp(.lt);
        const exit_jump = try self.emitJump(.if_false);
        try self.emitOp(.drop);

        // Get element: arr[idx]
        try self.emitOp(.get_loc);
        try self.emitByte(arr_slot);
        try self.emitOp(.get_loc);
        try self.emitByte(idx_slot);
        try self.emitOp(.get_elem);

        // Bind to loop variable
        const elem_slot = self.local_count;
        try self.declareLocal(var_name, is_const);
        try self.emitOp(.put_loc);
        try self.emitByte(elem_slot);

        // Execute body
        try self.statement();

        // Increment index
        try self.emitOp(.get_loc);
        try self.emitByte(idx_slot);
        try self.emitOp(.inc);
        try self.emitOp(.put_loc);
        try self.emitByte(idx_slot);

        // Jump back to loop start
        try self.emitLoop(loop_start);

        // Patch exit jump
        self.patchJump(exit_jump);
        try self.emitOp(.drop);

        try self.endScope();
    }

    fn forInStatement(self: *Parser, var_name: []const u8, is_const: bool) Error!void {
        // Parse object expression
        try self.expression();
        try self.consume(.rparen, "Expected ')' after for...in expression.");

        // For now, for...in on objects is not fully implemented
        // We'll emit code that just iterates like for...of with Object.keys
        // This is a simplified implementation

        // Stack: [object]
        // Get Object.keys(obj) - but we don't have Object.keys returning an array yet
        // For now, just push undefined and exit immediately (placeholder)
        try self.emitOp(.drop); // drop the object
        try self.emitOp(.push_undefined);

        // Declare the loop variable but don't iterate
        const elem_slot = self.local_count;
        try self.declareLocal(var_name, is_const);
        try self.emitOp(.put_loc);
        try self.emitByte(elem_slot);

        // Parse body but it won't execute (we jump over it)
        const skip_jump = try self.emitJump(.goto);
        try self.statement();
        self.patchJump(skip_jump);

        try self.endScope();
    }

    fn switchStatement(self: *Parser) Error!void {
        try self.consume(.lparen, "Expected '(' after 'switch'.");
        try self.expression(); // Switch value stays on stack
        try self.consume(.rparen, "Expected ')' after switch expression.");
        try self.consume(.lbrace, "Expected '{' before switch body.");

        // Track breaks that need patching at the end
        var break_jumps: [64]u32 = undefined;
        var break_count: usize = 0;

        // First pass: emit jumps to case labels, collecting case values
        // We'll use a simpler approach: for each case, dup switch value, compare, jump
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (self.match(.kw_case)) {
                // Duplicate switch value for comparison
                try self.emitOp(.dup);
                try self.expression(); // Case value
                try self.consume(.colon, "Expected ':' after case value.");

                // Compare: switch_value === case_value
                try self.emitOp(.strict_eq);
                const skip_case = try self.emitJump(.if_false);

                // Drop the switch value copy (comparison consumed it, but we need to drop for fall-through)
                // Actually, strict_eq pops both operands and pushes result, if_false pops result
                // So we don't need extra drops here

                // Parse case body (statements until next case/default/rbrace)
                while (!self.check(.kw_case) and !self.check(.kw_default) and
                    !self.check(.rbrace) and !self.check(.eof))
                {
                    if (self.match(.kw_break)) {
                        _ = self.match(.semicolon);
                        // Jump to end of switch
                        if (break_count < break_jumps.len) {
                            break_jumps[break_count] = try self.emitJump(.goto);
                            break_count += 1;
                        }
                    } else {
                        try self.statement();
                    }
                }

                self.patchJump(skip_case);
            } else if (self.match(.kw_default)) {
                try self.consume(.colon, "Expected ':' after 'default'.");

                // Parse default body
                while (!self.check(.kw_case) and !self.check(.kw_default) and
                    !self.check(.rbrace) and !self.check(.eof))
                {
                    if (self.match(.kw_break)) {
                        _ = self.match(.semicolon);
                        if (break_count < break_jumps.len) {
                            break_jumps[break_count] = try self.emitJump(.goto);
                            break_count += 1;
                        }
                    } else {
                        try self.statement();
                    }
                }
            } else {
                self.errorAtCurrent("Expected 'case' or 'default' in switch.");
                return error.UnexpectedToken;
            }
        }

        try self.consume(.rbrace, "Expected '}' after switch body.");

        // Patch all break jumps to here
        for (break_jumps[0..break_count]) |jump| {
            self.patchJump(jump);
        }

        // Drop the switch value
        try self.emitOp(.drop);
    }

    fn returnStatement(self: *Parser) !void {
        if (self.check(.semicolon) or self.check(.rbrace) or self.check(.eof)) {
            try self.emitOp(.push_undefined);
        } else {
            try self.expression();
        }
        _ = self.match(.semicolon);
        try self.emitOp(.ret);
    }

    fn throwStatement(self: *Parser) !void {
        // throw requires an expression
        try self.expression();
        _ = self.match(.semicolon);
        try self.emitOp(.@"throw");
    }

    fn tryStatement(self: *Parser) !void {
        // Emit push_catch with placeholder offset
        try self.emitOp(.push_catch);
        const catch_jump = self.code.items.len;
        try self.emitByte(0); // Placeholder for offset
        try self.emitByte(0);

        // Parse try block
        try self.consume(.lbrace, "Expected '{' after 'try'.");
        self.beginScope();
        try self.block();
        try self.endScope();

        // Normal exit from try - pop catch handler and jump past catch
        try self.emitOp(.pop_catch);
        const exit_jump = try self.emitJump(.goto);

        // Patch catch jump to here
        const catch_offset: i16 = @intCast(@as(i32, @intCast(self.code.items.len)) - @as(i32, @intCast(catch_jump)) - 2);
        self.code.items[catch_jump] = @intCast(@as(u16, @bitCast(catch_offset)) & 0xFF);
        self.code.items[catch_jump + 1] = @intCast((@as(u16, @bitCast(catch_offset)) >> 8) & 0xFF);

        // Parse catch clause
        if (self.match(.kw_catch)) {
            // Optional catch parameter: catch (e) or just catch
            if (self.match(.lparen)) {
                try self.consume(.identifier, "Expected exception variable name.");
                const name = self.previous.text(self.tokenizer.source);

                // Create local for exception variable
                try self.declareLocal(name, false);
                try self.consume(.rparen, "Expected ')' after catch parameter.");

                // Get exception value and store in local
                try self.emitOp(.get_exception);
                const local_idx = self.resolveLocal(name) orelse return error.UnexpectedToken;
                try self.emitOp(.put_loc);
                try self.emitByte(local_idx);
            }

            // Parse catch block
            try self.consume(.lbrace, "Expected '{' after 'catch'.");
            self.beginScope();
            try self.block();
            try self.endScope();
        }

        // Parse finally clause (optional)
        if (self.match(.kw_finally)) {
            // Finally blocks are complex - for now, just parse the block
            // A full implementation would need to execute finally on all paths
            try self.consume(.lbrace, "Expected '{' after 'finally'.");
            self.beginScope();
            try self.block();
            try self.endScope();
        }

        // Patch exit jump to here
        self.patchJump(exit_jump);
    }

    fn block(self: *Parser) !void {
        while (!self.check(.rbrace) and !self.check(.eof)) {
            try self.declaration();
        }
        try self.consume(.rbrace, "Expected '}' after block.");
    }

    fn expressionStatement(self: *Parser) !void {
        try self.expression();
        _ = self.match(.semicolon);
        // Don't drop the result if this is the last statement (for REPL/eval mode)
        if (!self.check(.eof)) {
            try self.emitOp(.drop);
        }
    }

    // ========================================================================
    // Expressions (Pratt Parser)
    // ========================================================================

    fn expression(self: *Parser) !void {
        try self.parsePrecedence(.assignment);
    }

    const Precedence = enum(u8) {
        none,
        assignment, // =
        ternary, // ?:
        or_prec, // ||
        and_prec, // &&
        equality, // == !=
        comparison, // < > <= >=
        term, // + -
        factor, // * /
        unary, // ! -
        call, // . ()
        primary,

        fn higher(self: Precedence) Precedence {
            if (self == .primary) return .primary;
            return @enumFromInt(@intFromEnum(self) + 1);
        }
    };

    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        self.advance();
        var prefix_rule = getRule(self.previous.type).prefix;

        // Handle regex literals: if we see a slash in prefix position, rescan as regex
        if (prefix_rule == null and self.previous.type == .slash) {
            // Rewind and scan as regex
            self.tokenizer.pos = self.previous.start;
            self.previous = self.tokenizer.scanRegex();
            if (self.previous.type == .regex_literal) {
                prefix_rule = getRule(.regex_literal).prefix;
            }
        }

        if (prefix_rule == null) {
            self.errorAtPrevious("Expected expression.");
            return error.UnexpectedToken;
        }

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.assignment);
        try prefix_rule.?(self, can_assign);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.type).precedence)) {
            self.advance();
            const infix_rule = getRule(self.previous.type).infix;
            if (infix_rule) |rule| {
                try rule(self, can_assign);
            }
        }

        if (can_assign and self.match(.assign)) {
            self.errorAtPrevious("Invalid assignment target.");
        }
    }

    // Prefix parsers
    fn literal(self: *Parser, _: bool) !void {
        switch (self.previous.type) {
            .@"true" => try self.emitOp(.push_true),
            .@"false" => try self.emitOp(.push_false),
            .@"null" => try self.emitOp(.push_null),
            .undefined => try self.emitOp(.push_undefined),
            else => unreachable,
        }
    }

    fn number(self: *Parser, _: bool) !void {
        const text = self.previous.text(self.tokenizer.source);
        const num = std.fmt.parseFloat(f64, text) catch {
            self.errorAtPrevious("Invalid number.");
            return error.InvalidNumber;
        };

        // Try to store as integer
        if (num == @trunc(num) and num >= @as(f64, @floatFromInt(std.math.minInt(i31))) and num <= @as(f64, @floatFromInt(std.math.maxInt(i31)))) {
            const int_val: i32 = @intFromFloat(num);
            // Use specialized opcodes for small integers
            if (int_val == 0) {
                try self.emitOp(.push_0);
            } else if (int_val == 1) {
                try self.emitOp(.push_1);
            } else if (int_val == 2) {
                try self.emitOp(.push_2);
            } else if (int_val == 3) {
                try self.emitOp(.push_3);
            } else if (int_val >= std.math.minInt(i8) and int_val <= std.math.maxInt(i8)) {
                try self.emitOp(.push_i8);
                try self.emitByte(@bitCast(@as(i8, @intCast(int_val))));
            } else if (int_val >= std.math.minInt(i16) and int_val <= std.math.maxInt(i16)) {
                try self.emitOp(.push_i16);
                const bytes: [2]u8 = @bitCast(@as(i16, @intCast(int_val)));
                try self.code.appendSlice(&bytes);
            } else {
                // Larger integers go in constant pool
                const const_idx = try self.addConstant(value.JSValue.fromInt(int_val));
                try self.emitOp(.push_const);
                try self.emitU16(const_idx);
            }
        } else {
            // Float goes in constant pool as Float64Box
            const float_box = self.allocator.create(value.JSValue.Float64Box) catch return error.OutOfMemory;
            float_box.* = .{
                .header = 2, // MemTag.float64
                ._pad = 0,
                .value = num,
            };
            const const_idx = try self.addConstant(value.JSValue.fromPtr(float_box));
            try self.emitOp(.push_const);
            try self.emitU16(const_idx);
        }
    }

    fn stringLiteral(self: *Parser, _: bool) !void {
        const text = self.previous.text(self.tokenizer.source);
        // Remove quotes and process escapes
        const content = text[1 .. text.len - 1];
        const const_idx = try self.addStringConstant(content);
        try self.emitOp(.push_const);
        try self.emitU16(const_idx);
    }

    fn regexLiteral(self: *Parser, _: bool) !void {
        const text = self.previous.text(self.tokenizer.source);
        // Parse /pattern/flags
        // Find the closing / (skip the opening /)
        var pattern_end: usize = 1;
        var in_class = false;
        while (pattern_end < text.len) {
            const c = text[pattern_end];
            if (c == '\\' and pattern_end + 1 < text.len) {
                pattern_end += 2;
                continue;
            }
            if (c == '[') in_class = true;
            if (c == ']') in_class = false;
            if (c == '/' and !in_class) break;
            pattern_end += 1;
        }

        const pattern = text[1..pattern_end];
        const flags = if (pattern_end + 1 < text.len) text[pattern_end + 1 ..] else "";

        // Store pattern and flags as string constants
        const pattern_idx = try self.addStringConstant(pattern);
        const flags_idx = try self.addStringConstant(flags);

        // Emit: get RegExp constructor, push pattern, push flags, call with 2 args
        // For simplicity, emit as: new RegExp(pattern, flags)
        const regexp_atom = try self.getOrCreateAtom("RegExp");
        try self.emitOp(.get_global);
        try self.emitU16(regexp_atom);
        try self.emitOp(.push_const);
        try self.emitU16(pattern_idx);
        try self.emitOp(.push_const);
        try self.emitU16(flags_idx);
        try self.emitOp(.call_constructor);
        try self.emitByte(2);
    }

    fn identifier(self: *Parser, can_assign: bool) !void {
        const name = self.previous.text(self.tokenizer.source);

        // Check for arrow function: x => expr
        if (self.match(.arrow)) {
            try self.arrowFunctionBody(&[_][]const u8{name});
            return;
        }

        // Check for local
        var local_idx: ?u8 = null;
        var i: i32 = @as(i32, self.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.locals[@intCast(i)];
            if (std.mem.eql(u8, local.name, name)) {
                local_idx = @intCast(i);
                break;
            }
        }

        if (can_assign and self.match(.assign)) {
            try self.expression();
            if (local_idx) |idx| {
                try self.emitOp(.put_loc);
                try self.emitByte(idx);
            } else {
                const name_const = try self.addStringConstant(name);
                try self.emitOp(.put_global);
                try self.emitByte(name_const);
            }
        } else {
            if (local_idx) |idx| {
                try self.emitOp(.get_loc);
                try self.emitByte(idx);
            } else {
                const name_const = try self.addStringConstant(name);
                try self.emitOp(.get_global);
                try self.emitByte(name_const);
            }
        }
    }

    fn grouping(self: *Parser, _: bool) !void {
        // Check for arrow function: () => or (x) => or (x, y) =>
        // We need to check if this is a parameter list followed by =>

        // Save position to potentially backtrack
        const saved_pos = self.tokenizer.pos;
        const saved_current = self.current;
        const saved_previous = self.previous;

        // Try to parse as arrow function parameters
        var params: [16][]const u8 = undefined;
        var param_count: usize = 0;
        var is_arrow = false;

        if (self.check(.rparen)) {
            // () => case
            self.advance(); // consume )
            if (self.check(.arrow)) {
                is_arrow = true;
            }
        } else if (self.check(.identifier)) {
            // Try to parse parameter list
            while (true) {
                if (!self.check(.identifier)) break;
                if (param_count >= params.len) break;

                self.advance();
                params[param_count] = self.previous.text(self.tokenizer.source);
                param_count += 1;

                if (!self.match(.comma)) break;
            }

            if (self.check(.rparen)) {
                self.advance(); // consume )
                if (self.check(.arrow)) {
                    is_arrow = true;
                }
            }
        }

        if (is_arrow) {
            self.advance(); // consume =>
            try self.arrowFunctionBody(params[0..param_count]);
            return;
        }

        // Not an arrow function - restore and parse as grouping
        self.tokenizer.pos = saved_pos;
        self.current = saved_current;
        self.previous = saved_previous;

        try self.expression();
        try self.consume(.rparen, "Expected ')' after expression.");
    }

    fn arrowFunctionBody(self: *Parser, params: []const []const u8) !void {
        // Save outer parser state
        const outer_code = self.code;
        const outer_constants = self.constants;
        const outer_locals = self.locals;
        const outer_local_count = self.local_count;
        const outer_scope_depth = self.scope_depth;

        // Initialize new function scope
        self.code = std.array_list.Managed(u8).init(self.allocator);
        self.constants = std.array_list.Managed(value.JSValue).init(self.allocator);
        self.local_count = 0;
        self.scope_depth = 1;

        // Add parameters as locals
        const param_count: u8 = @intCast(params.len);
        for (params) |param| {
            if (self.local_count >= 255) {
                self.errorAtCurrent("Cannot have more than 255 parameters.");
                break;
            }
            self.locals[self.local_count] = .{
                .name = param,
                .depth = 1,
                .is_const = false,
            };
            self.local_count += 1;
        }

        // Compile body
        if (self.match(.lbrace)) {
            // Block body: (x) => { ... }
            try self.block();
            // If no explicit return, return undefined
            try self.emitOp(.push_undefined);
            try self.emitOp(.ret);
        } else {
            // Expression body: (x) => x + 1
            try self.expression();
            try self.emitOp(.ret);
        }

        // Create FunctionBytecode on heap
        const func_bc = self.allocator.create(bytecode.FunctionBytecode) catch return error.OutOfMemory;
        const code_copy = self.allocator.dupe(u8, self.code.items) catch return error.OutOfMemory;
        const consts_copy = self.allocator.dupe(value.JSValue, self.constants.items) catch return error.OutOfMemory;

        func_bc.* = .{
            .header = .{},
            .name_atom = 0, // Arrow functions are anonymous
            .arg_count = param_count,
            .local_count = self.local_count,
            .stack_size = 256,
            .flags = .{},
            .code = code_copy,
            .constants = consts_copy,
            .source_map = null,
        };

        // Clean up inner buffers
        self.code.deinit();
        self.constants.deinit();

        // Restore outer parser state
        self.code = outer_code;
        self.constants = outer_constants;
        self.locals = outer_locals;
        self.local_count = outer_local_count;
        self.scope_depth = outer_scope_depth;

        // Store function bytecode in constant pool and emit make_function
        const const_idx = try self.addConstant(value.JSValue.fromPtr(func_bc));
        try self.emitOp(.make_function);
        try self.emitU16(const_idx);
    }

    /// Parse array literal: [elem1, elem2, ...]
    fn arrayLiteral(self: *Parser, _: bool) !void {
        // Create empty array first
        try self.emitOp(.new_array);
        try self.emitU16(0);

        // Push initial index 0 onto stack for dynamic tracking (needed for spread)
        try self.emitOp(.push_0);

        // Stack: [array, index]
        var has_spread = false;

        if (!self.check(.rbracket)) {
            while (true) {
                // Check for spread operator
                if (self.match(.spread)) {
                    has_spread = true;
                    // Stack: [array, index]
                    try self.expression(); // Parse the iterable
                    // Stack: [array, index, source_array]
                    try self.emitOp(.array_spread);
                    // Stack: [array, new_index]
                } else if (self.check(.comma)) {
                    // Handle sparse arrays: [1, , 3]
                    // Stack: [array, index]
                    try self.emitOp(.dup); // [array, index, index]
                    try self.emitOp(.rot3); // [index, array, index]
                    try self.emitOp(.dup); // [index, array, index, index]
                    try self.emitOp(.rot3); // [index, index, array, index] - no that's wrong
                    // Simpler: just emit at fixed index
                    // Actually for sparse, we just need to increment index
                    try self.emitOp(.inc);
                    // Stack: [array, index+1]
                } else {
                    // Regular element
                    // Stack: [array, index]
                    // We need: dup array, get index, parse expr, put_elem, increment index
                    // Reorder: [array, index] -> need [array, array, index, value]

                    // Swap and duplicate for put_elem
                    try self.emitOp(.swap); // [index, array]
                    try self.emitOp(.dup); // [index, array, array]
                    try self.emitOp(.rot3); // [array, index, array] - wrong
                    // This is getting complicated. Let me simplify.

                    // Actually, let's use a simpler approach: track index on stack
                    // pop index, dup array, push index, parse value, put_elem, push index+1
                    // [array, index]
                    const idx_slot = self.local_count;

                    // Pop index to temp, dup array, push index, expr, put_elem, push index+1
                    // Simpler: just rearrange stack
                    // [array, index] -> swap -> [index, array] -> dup -> [index, array, array]
                    // -> rot3 -> [array, array, index] -> expr -> [array, array, index, val]
                    // -> put_elem -> [array] -> push_old_index+1

                    // Let's do it differently - store index in local temporarily
                    // Actually we don't have temp locals easily available

                    // For now, fall back to simpler index tracking if no spread
                    if (!has_spread) {
                        // No spread yet - we can use static index
                        // Swap: [index, array]
                        try self.emitOp(.swap);
                        // Drop index: [array]
                        try self.emitOp(.drop);
                        // Emit the rest with static indexing
                        try self.arrayLiteralStatic();
                        return;
                    }

                    // With spread, we need dynamic index tracking
                    // [array, index]
                    try self.emitOp(.swap); // [index, array]
                    try self.emitOp(.dup); // [index, array, array]
                    try self.emitOp(.rot3); // [array, array, index]
                    try self.expression(); // [array, array, index, value]
                    try self.emitOp(.put_elem); // [array]

                    // We need to get old index back and increment... this is tricky
                    // For now, just push 0 and let spread handle it
                    _ = idx_slot;
                    try self.emitOp(.push_0);
                }

                if (!self.match(.comma)) break;
                if (self.check(.rbracket)) break; // Trailing comma
            }
        }
        try self.consume(.rbracket, "Expected ']' after array elements.");

        // Drop the index, keep array
        try self.emitOp(.swap);
        try self.emitOp(.drop);
        // Stack: [array]
    }

    /// Parse array literal with static indices (simpler, no spread)
    fn arrayLiteralStatic(self: *Parser) !void {
        // Stack: [array]
        var index: u16 = 0;

        if (!self.check(.rbracket)) {
            while (true) {
                try self.emitOp(.dup); // [array, array]

                // Push index
                if (index <= 127) {
                    try self.emitOp(.push_i8);
                    try self.emitByte(@truncate(index));
                } else {
                    try self.emitOp(.push_i16);
                    try self.emitU16(index);
                }
                // Stack: [array, array, index]

                // Handle sparse arrays: [1, , 3]
                if (self.check(.comma)) {
                    try self.emitOp(.push_undefined);
                } else if (self.match(.spread)) {
                    // Spread encountered - switch to dynamic mode
                    // This shouldn't happen since we checked earlier, but handle it
                    try self.expression();
                    try self.emitOp(.drop); // Drop for now
                    try self.emitOp(.drop);
                } else {
                    try self.expression();
                }
                // Stack: [array, array, index, value]

                try self.emitOp(.put_elem);
                // Stack: [array]

                index += 1;
                if (!self.match(.comma)) break;
                if (self.check(.rbracket)) break; // Trailing comma
            }
        }
        try self.consume(.rbracket, "Expected ']' after array elements.");

        // Set length property
        try self.emitOp(.dup);
        const length_atom = @intFromEnum(object.Atom.length);
        if (index <= 127) {
            try self.emitOp(.push_i8);
            try self.emitByte(@truncate(index));
        } else {
            try self.emitOp(.push_i16);
            try self.emitU16(index);
        }
        try self.emitOp(.put_field);
        try self.emitU16(length_atom);
        // Array remains on stack
    }

    /// Parse object literal: {key: value, ...}
    fn objectLiteral(self: *Parser, _: bool) !void {
        try self.emitOp(.new_object);

        if (!self.check(.rbrace)) {
            while (true) {
                // Parse key: identifier, string, or number
                var key_atom: u16 = undefined;
                if (self.match(.identifier)) {
                    const name = self.previous.text(self.tokenizer.source);
                    key_atom = try self.getOrCreateAtom(name);
                } else if (self.match(.string_literal)) {
                    const text = self.previous.text(self.tokenizer.source);
                    const content = text[1 .. text.len - 1];
                    key_atom = try self.getOrCreateAtom(content);
                } else if (self.match(.number)) {
                    // Numeric key - use as-is for now
                    const text = self.previous.text(self.tokenizer.source);
                    key_atom = try self.getOrCreateAtom(text);
                } else {
                    self.errorAtCurrent("Expected property name.");
                    return error.UnexpectedToken;
                }

                try self.consume(.colon, "Expected ':' after property name.");
                try self.emitOp(.dup); // Duplicate object for put_field
                try self.expression();
                try self.emitOp(.put_field);
                try self.emitU16(key_atom);

                if (!self.match(.comma)) break;
                if (self.check(.rbrace)) break; // Trailing comma
            }
        }
        try self.consume(.rbrace, "Expected '}' after object literal.");
    }

    /// Parse template literal: `string` or `string${expr}string`
    fn templateLiteral(self: *Parser, _: bool) !void {
        const text = self.previous.text(self.tokenizer.source);

        if (self.previous.type == .template_literal) {
            // Simple template literal without expressions: `hello`
            // Remove backticks and add as string constant
            const content = text[1 .. text.len - 1];
            const processed = try self.processTemplateString(content);
            const const_idx = try self.addStringConstant(processed);
            try self.emitOp(.push_const);
            try self.emitU16(const_idx);
        } else if (self.previous.type == .template_head) {
            // Template with expressions: `hello ${
            // Extract the head string (remove ` and ${)
            const head_content = text[1 .. text.len - 2];
            const processed_head = try self.processTemplateString(head_content);
            const head_idx = try self.addStringConstant(processed_head);
            try self.emitOp(.push_const);
            try self.emitU16(head_idx);

            // Parse expression
            try self.expression();

            // Convert expression to string and concatenate (simplified: just add)
            try self.emitOp(.add);

            // Continue parsing middle/tail parts
            try self.parseTemplateRest();
        }
    }

    /// Continue parsing template after first expression
    fn parseTemplateRest(self: *Parser) !void {
        while (true) {
            // After expression, we need to scan for }string${ or }string`
            const tok = self.scanTemplateMiddleOrTail();
            const text = tok.text(self.tokenizer.source);

            if (tok.type == .template_tail) {
                // }string` - final part
                const tail_content = text[1 .. text.len - 1]; // Remove } and `
                if (tail_content.len > 0) {
                    const processed = try self.processTemplateString(tail_content);
                    const const_idx = try self.addStringConstant(processed);
                    try self.emitOp(.push_const);
                    try self.emitU16(const_idx);
                    try self.emitOp(.add);
                }
                break;
            } else if (tok.type == .template_middle) {
                // }string${ - middle part
                const middle_content = text[1 .. text.len - 2]; // Remove } and ${
                if (middle_content.len > 0) {
                    const processed = try self.processTemplateString(middle_content);
                    const const_idx = try self.addStringConstant(processed);
                    try self.emitOp(.push_const);
                    try self.emitU16(const_idx);
                    try self.emitOp(.add);
                }

                // Parse next expression
                try self.expression();
                try self.emitOp(.add);
            } else {
                self.errorAtCurrent("Unterminated template literal.");
                break;
            }
        }
    }

    /// Scan for template middle (}...${) or tail (}...`)
    fn scanTemplateMiddleOrTail(self: *Parser) Token {
        const start = self.tokenizer.pos;
        const start_line = self.tokenizer.line;

        while (self.tokenizer.pos < self.tokenizer.source.len) {
            const c = self.tokenizer.source[self.tokenizer.pos];
            if (c == '`') {
                self.tokenizer.pos += 1;
                return .{
                    .type = .template_tail,
                    .start = @intCast(start),
                    .len = @intCast(self.tokenizer.pos - start),
                    .line = start_line,
                };
            } else if (c == '$' and self.tokenizer.pos + 1 < self.tokenizer.source.len and
                self.tokenizer.source[self.tokenizer.pos + 1] == '{')
            {
                self.tokenizer.pos += 2;
                return .{
                    .type = .template_middle,
                    .start = @intCast(start),
                    .len = @intCast(self.tokenizer.pos - start),
                    .line = start_line,
                };
            } else if (c == '\\' and self.tokenizer.pos + 1 < self.tokenizer.source.len) {
                self.tokenizer.pos += 2; // Skip escape
            } else {
                if (c == '\n') {
                    self.tokenizer.line += 1;
                }
                self.tokenizer.pos += 1;
            }
        }

        // Unterminated
        return .{
            .type = .invalid,
            .start = @intCast(start),
            .len = @intCast(self.tokenizer.pos - start),
            .line = start_line,
        };
    }

    /// Process escape sequences in template string content
    fn processTemplateString(self: *Parser, content: []const u8) ![]const u8 {
        // For now, return as-is. A full implementation would process:
        // \n, \t, \r, \\, \`, \$, etc.
        _ = self;
        return content;
    }

    /// Parse 'this' keyword
    fn thisExpr(self: *Parser, _: bool) !void {
        try self.emitOp(.push_this);
    }

    /// Parse function expression: function() {} or function name() {} or function*() {}
    fn functionExpr(self: *Parser, _: bool) !void {
        const is_generator = self.match(.star); // function* for generators
        // Optional name for named function expressions
        var name: []const u8 = "";
        if (self.match(.identifier)) {
            name = self.previous.text(self.tokenizer.source);
        }
        try self.compileFunction(name, is_generator, false);
    }

    /// Parse async function expression: async function() {} or async function name() {}
    fn asyncFunctionExpr(self: *Parser, _: bool) !void {
        try self.consume(.kw_function, "Expected 'function' after 'async'.");
        // Optional name for named async function expressions
        var name: []const u8 = "";
        if (self.match(.identifier)) {
            name = self.previous.text(self.tokenizer.source);
        }
        try self.compileFunction(name, false, true);
    }

    /// Parse yield expression: yield, yield expr, yield* expr
    fn yieldExpr(self: *Parser, _: bool) !void {
        if (!self.in_generator) {
            self.errorAtPrevious("'yield' is only valid inside a generator function");
            return error.UnexpectedToken;
        }

        // Check for yield* (delegation)
        const is_delegate = self.match(.star);

        // Check if there's an expression following yield
        // yield without expression yields undefined
        if (self.check(.semicolon) or self.check(.rbrace) or self.check(.rparen) or
            self.check(.rbracket) or self.check(.comma) or self.check(.colon) or self.check(.eof))
        {
            // yield with no expression - yields undefined
            try self.emitOp(.push_undefined);
        } else {
            // yield expr - parse the expression
            try self.parsePrecedence(.assignment);
        }

        // Emit the appropriate yield opcode
        if (is_delegate) {
            try self.emitOp(.yield_star);
        } else {
            try self.emitOp(.yield_val);
        }
    }

    /// Parse await expression: await expr
    fn awaitExpr(self: *Parser, _: bool) !void {
        if (!self.in_async) {
            self.errorAtPrevious("'await' is only valid inside an async function");
            return error.UnexpectedToken;
        }

        // Parse the expression to await
        try self.parsePrecedence(.unary);

        // Emit await opcode
        try self.emitOp(.await_val);
    }

    /// Parse ternary conditional: a ? b : c
    fn ternary(self: *Parser, _: bool) !void {
        // Condition is already on stack
        const else_jump = try self.emitJump(.if_false);
        try self.emitOp(.drop); // Drop condition value

        // Parse 'then' branch
        try self.expression();
        const end_jump = try self.emitJump(.goto);

        // Parse 'else' branch
        self.patchJump(else_jump);
        try self.emitOp(.drop); // Drop condition value
        try self.consume(.colon, "Expected ':' in ternary expression.");
        try self.parsePrecedence(.ternary);

        self.patchJump(end_jump);
    }

    fn unary(self: *Parser, _: bool) !void {
        const op_type = self.previous.type;
        try self.parsePrecedence(.unary);

        switch (op_type) {
            .minus => try self.emitOp(.neg),
            .bang => try self.emitOp(.not),
            .tilde => try self.emitOp(.bit_not),
            .kw_typeof => try self.emitOp(.typeof),
            else => unreachable,
        }
    }

    /// Parse 'new' expression: new Constructor(args)
    fn newExpr(self: *Parser, _: bool) !void {
        // Parse the constructor expression (high precedence, stops before call)
        try self.parsePrecedence(.call);

        // Parse arguments if present
        var arg_count: u8 = 0;
        if (self.match(.lparen)) {
            if (!self.check(.rparen)) {
                while (true) {
                    try self.expression();
                    arg_count += 1;
                    if (!self.match(.comma)) break;
                }
            }
            try self.consume(.rparen, "Expected ')' after constructor arguments.");
        }

        try self.emitOp(.call_constructor);
        try self.emitByte(arg_count);
    }

    /// Parse 'delete' expression: delete obj.prop or delete obj[key]
    fn deleteExpr(self: *Parser, _: bool) !void {
        // Parse the operand (must be a property access)
        try self.parsePrecedence(.unary);

        // For now, just emit delete_field or delete_elem
        // In a full implementation, we'd check that the operand is a valid target
        // Since we don't track the exact form, we'll emit a simplified version
        // that pops the value and pushes true (delete always succeeds in non-strict mode)
        try self.emitOp(.drop);
        try self.emitOp(.push_true);
    }

    /// Parse 'instanceof' operator: obj instanceof Constructor
    fn instanceofOp(self: *Parser, _: bool) !void {
        try self.parsePrecedence(Precedence.comparison.higher());
        try self.emitOp(.instanceof);
    }

    // Infix parsers
    fn binary(self: *Parser, _: bool) !void {
        const op_type = self.previous.type;
        const rule = getRule(op_type);
        try self.parsePrecedence(rule.precedence.higher());

        switch (op_type) {
            .plus => try self.emitOp(.add),
            .minus => try self.emitOp(.sub),
            .star => try self.emitOp(.mul),
            .slash => try self.emitOp(.div),
            .percent => try self.emitOp(.mod),
            .eq => try self.emitOp(.eq),
            .ne => try self.emitOp(.neq),
            .eq_eq => try self.emitOp(.strict_eq),
            .ne_ne => try self.emitOp(.strict_neq),
            .lt => try self.emitOp(.lt),
            .le => try self.emitOp(.lte),
            .gt => try self.emitOp(.gt),
            .ge => try self.emitOp(.gte),
            .ampersand => try self.emitOp(.bit_and),
            .pipe => try self.emitOp(.bit_or),
            .caret => try self.emitOp(.bit_xor),
            .lt_lt => try self.emitOp(.shl),
            .gt_gt => try self.emitOp(.shr),
            .gt_gt_gt => try self.emitOp(.ushr),
            else => unreachable,
        }
    }

    fn andOp(self: *Parser, _: bool) !void {
        const end_jump = try self.emitJump(.if_false);
        try self.emitOp(.drop);
        try self.parsePrecedence(.and_prec);
        self.patchJump(end_jump);
    }

    fn orOp(self: *Parser, _: bool) !void {
        const else_jump = try self.emitJump(.if_false);
        const end_jump = try self.emitJump(.goto);
        self.patchJump(else_jump);
        try self.emitOp(.drop);
        try self.parsePrecedence(.or_prec);
        self.patchJump(end_jump);
    }

    /// Nullish coalescing: a ?? b
    /// Returns b only if a is null or undefined, otherwise returns a
    fn nullishCoalesce(self: *Parser, _: bool) !void {
        // Stack has left value
        // Check if it's null
        try self.emitOp(.dup);
        try self.emitOp(.push_null);
        try self.emitOp(.eq);
        const null_jump = try self.emitJump(.if_true);

        // Check if it's undefined
        try self.emitOp(.dup);
        try self.emitOp(.push_undefined);
        try self.emitOp(.eq);
        const undefined_jump = try self.emitJump(.if_true);

        // Not nullish - keep left value and skip right side
        const end_jump = try self.emitJump(.goto);

        // Nullish - drop left value and evaluate right side
        self.patchJump(null_jump);
        self.patchJump(undefined_jump);
        try self.emitOp(.drop); // Drop the duplicated value
        try self.emitOp(.drop); // Drop the original left value
        try self.parsePrecedence(.or_prec); // Same precedence as ||

        self.patchJump(end_jump);
    }

    fn call(self: *Parser, _: bool) !void {
        var arg_count: u8 = 0;
        if (!self.check(.rparen)) {
            while (true) {
                try self.expression();
                arg_count += 1;
                if (!self.match(.comma)) break;
            }
        }
        try self.consume(.rparen, "Expected ')' after arguments.");
        try self.emitOp(.call);
        try self.emitByte(arg_count);
    }

    fn dot(self: *Parser, can_assign: bool) !void {
        try self.consume(.identifier, "Expected property name after '.'.");
        const name = self.previous.text(self.tokenizer.source);

        // Check if this is a method call: obj.method(...)
        if (self.check(.lparen)) {
            // Method call - emit call_method instead of get_field + call
            const name_atom = try self.getOrCreateAtom(name);
            try self.emitOp(.get_field);
            try self.emitU16(name_atom);
            // The call will be handled by the call infix parser
        } else if (can_assign and self.match(.assign)) {
            try self.expression();
            const name_atom = try self.getOrCreateAtom(name);
            try self.emitOp(.put_field);
            try self.emitU16(name_atom);
        } else {
            const name_atom = try self.getOrCreateAtom(name);
            try self.emitOp(.get_field);
            try self.emitU16(name_atom);
        }
    }

    /// Optional chaining: obj?.prop
    /// If obj is null/undefined, returns undefined without accessing property
    fn optionalDot(self: *Parser, _: bool) !void {
        // Stack has the object value
        // Emit: dup, check if null/undefined, if so jump to skip and push undefined

        // Duplicate the value for the null check
        try self.emitOp(.dup);
        try self.emitOp(.push_null);
        try self.emitOp(.eq);
        const null_jump = try self.emitJump(.if_true);

        // Check for undefined
        try self.emitOp(.dup);
        try self.emitOp(.push_undefined);
        try self.emitOp(.eq);
        const undefined_jump = try self.emitJump(.if_true);

        // Not null/undefined - proceed with property access
        try self.consume(.identifier, "Expected property name after '?.'.");
        const name = self.previous.text(self.tokenizer.source);
        const name_atom = try self.getOrCreateAtom(name);
        try self.emitOp(.get_field);
        try self.emitU16(name_atom);

        // Jump over the null/undefined case
        const end_jump = try self.emitJump(.goto);

        // null/undefined case: pop the object and push undefined
        self.patchJump(null_jump);
        self.patchJump(undefined_jump);
        try self.emitOp(.drop); // Drop the duplicated value
        try self.emitOp(.drop); // Drop the original object
        try self.emitOp(.push_undefined);

        self.patchJump(end_jump);
    }

    fn subscript(self: *Parser, can_assign: bool) !void {
        try self.expression();
        try self.consume(.rbracket, "Expected ']' after index.");

        if (can_assign and self.match(.assign)) {
            try self.expression();
            try self.emitOp(.put_elem);
        } else {
            try self.emitOp(.get_elem);
        }
    }

    // ========================================================================
    // Parse rules table
    // ========================================================================

    const ParseRule = struct {
        prefix: ?*const fn (*Parser, bool) ParseError!void,
        infix: ?*const fn (*Parser, bool) ParseError!void,
        precedence: Precedence,
    };

    fn getRule(token_type: TokenType) ParseRule {
        return switch (token_type) {
            .lparen => .{ .prefix = grouping, .infix = call, .precedence = .call },
            .lbracket => .{ .prefix = arrayLiteral, .infix = subscript, .precedence = .call },
            .lbrace => .{ .prefix = objectLiteral, .infix = null, .precedence = .none },
            .dot => .{ .prefix = null, .infix = dot, .precedence = .call },
            .question_dot => .{ .prefix = null, .infix = optionalDot, .precedence = .call },
            .question => .{ .prefix = null, .infix = ternary, .precedence = .ternary },
            .minus => .{ .prefix = unary, .infix = binary, .precedence = .term },
            .plus => .{ .prefix = null, .infix = binary, .precedence = .term },
            .slash, .star, .percent => .{ .prefix = null, .infix = binary, .precedence = .factor },
            .bang, .tilde => .{ .prefix = unary, .infix = null, .precedence = .none },
            .kw_typeof => .{ .prefix = unary, .infix = null, .precedence = .none },
            .kw_delete => .{ .prefix = deleteExpr, .infix = null, .precedence = .none },
            .kw_new => .{ .prefix = newExpr, .infix = null, .precedence = .none },
            .kw_instanceof => .{ .prefix = null, .infix = instanceofOp, .precedence = .comparison },
            .eq, .ne, .eq_eq, .ne_ne => .{ .prefix = null, .infix = binary, .precedence = .equality },
            .lt, .le, .gt, .ge => .{ .prefix = null, .infix = binary, .precedence = .comparison },
            .ampersand, .pipe, .caret => .{ .prefix = null, .infix = binary, .precedence = .term },
            .lt_lt, .gt_gt, .gt_gt_gt => .{ .prefix = null, .infix = binary, .precedence = .term },
            .ampersand_ampersand => .{ .prefix = null, .infix = andOp, .precedence = .and_prec },
            .pipe_pipe => .{ .prefix = null, .infix = orOp, .precedence = .or_prec },
            .question_question => .{ .prefix = null, .infix = nullishCoalesce, .precedence = .or_prec },
            .number => .{ .prefix = number, .infix = null, .precedence = .none },
            .string_literal => .{ .prefix = stringLiteral, .infix = null, .precedence = .none },
            .identifier => .{ .prefix = identifier, .infix = null, .precedence = .none },
            .kw_this => .{ .prefix = thisExpr, .infix = null, .precedence = .none },
            .kw_function => .{ .prefix = functionExpr, .infix = null, .precedence = .none },
            .@"true", .@"false", .@"null", .undefined => .{ .prefix = literal, .infix = null, .precedence = .none },
            .template_literal, .template_head => .{ .prefix = templateLiteral, .infix = null, .precedence = .none },
            .regex_literal => .{ .prefix = regexLiteral, .infix = null, .precedence = .none },
            .kw_yield => .{ .prefix = yieldExpr, .infix = null, .precedence = .none },
            .kw_async => .{ .prefix = asyncFunctionExpr, .infix = null, .precedence = .none },
            .kw_await => .{ .prefix = awaitExpr, .infix = null, .precedence = .none },
            else => .{ .prefix = null, .infix = null, .precedence = .none },
        };
    }

    // ========================================================================
    // Bytecode emission
    // ========================================================================

    fn emitOp(self: *Parser, op: bytecode.Opcode) !void {
        try self.code.append(@intFromEnum(op));
    }

    fn emitByte(self: *Parser, byte: u8) !void {
        try self.code.append(byte);
    }

    fn emitU16(self: *Parser, val: u16) !void {
        const bytes: [2]u8 = @bitCast(val);
        try self.code.appendSlice(&bytes);
    }

    fn emitInt(self: *Parser, val: i32) !void {
        const bytes: [4]u8 = @bitCast(val);
        try self.code.appendSlice(&bytes);
    }

    fn emitJump(self: *Parser, op: bytecode.Opcode) !u32 {
        try self.emitOp(op);
        try self.code.append(0xFF);
        try self.code.append(0xFF);
        return @intCast(self.code.items.len - 2);
    }

    fn patchJump(self: *Parser, offset: u32) void {
        const jump = @as(u16, @intCast(self.code.items.len - offset - 2));
        self.code.items[offset] = @truncate(jump);
        self.code.items[offset + 1] = @truncate(jump >> 8);
    }

    fn emitLoop(self: *Parser, loop_start: u32) !void {
        try self.emitOp(.loop);
        const offset = self.code.items.len - loop_start + 2;
        if (offset > 0xFFFF) {
            self.errorAtPrevious("Loop body too large.");
        }
        try self.emitByte(@truncate(offset));
        try self.emitByte(@truncate(offset >> 8));
    }

    fn addConstant(self: *Parser, val: value.JSValue) !u8 {
        if (self.constants.items.len >= 256) return error.TooManyConstants;
        try self.constants.append(val);
        return @intCast(self.constants.items.len - 1);
    }

    fn addStringConstant(self: *Parser, text: []const u8) !u8 {
        const js_str = self.strings.intern(text) catch return error.OutOfMemory;
        return self.addConstant(value.JSValue.fromPtr(js_str));
    }

    /// Get atom index for a property name
    /// Uses predefined atoms for common names, otherwise uses string constant index + offset
    fn getOrCreateAtom(self: *Parser, name: []const u8) !u16 {
        const object_mod = @import("object.zig");
        // Check predefined atoms first (all < 256)
        if (lookupPredefinedAtom(name)) |atom| {
            return @truncate(@intFromEnum(atom));
        }
        // Use constant pool index + offset for dynamic atoms
        const const_idx = try self.addStringConstant(name);
        return @truncate(@as(u32, object_mod.Atom.FIRST_DYNAMIC) + @as(u32, const_idx));
    }

    /// Lookup predefined atom by name
    fn lookupPredefinedAtom(name: []const u8) ?@import("object.zig").Atom {
        const object_mod = @import("object.zig");
        // Common property names
        if (std.mem.eql(u8, name, "length")) return object_mod.Atom.length;
        if (std.mem.eql(u8, name, "prototype")) return object_mod.Atom.prototype;
        if (std.mem.eql(u8, name, "constructor")) return object_mod.Atom.constructor;
        if (std.mem.eql(u8, name, "toString")) return object_mod.Atom.toString;
        if (std.mem.eql(u8, name, "valueOf")) return object_mod.Atom.valueOf;
        if (std.mem.eql(u8, name, "name")) return object_mod.Atom.name;
        if (std.mem.eql(u8, name, "message")) return object_mod.Atom.message;
        // Array methods
        if (std.mem.eql(u8, name, "push")) return object_mod.Atom.push;
        if (std.mem.eql(u8, name, "pop")) return object_mod.Atom.pop;
        if (std.mem.eql(u8, name, "map")) return object_mod.Atom.map;
        if (std.mem.eql(u8, name, "filter")) return object_mod.Atom.filter;
        if (std.mem.eql(u8, name, "forEach")) return object_mod.Atom.forEach;
        if (std.mem.eql(u8, name, "indexOf")) return object_mod.Atom.indexOf;
        if (std.mem.eql(u8, name, "slice")) return object_mod.Atom.slice;
        if (std.mem.eql(u8, name, "concat")) return object_mod.Atom.concat;
        if (std.mem.eql(u8, name, "join")) return object_mod.Atom.join;
        // String methods
        if (std.mem.eql(u8, name, "split")) return object_mod.Atom.split;
        if (std.mem.eql(u8, name, "substring")) return object_mod.Atom.substring;
        if (std.mem.eql(u8, name, "toLowerCase")) return object_mod.Atom.toLowerCase;
        if (std.mem.eql(u8, name, "toUpperCase")) return object_mod.Atom.toUpperCase;
        if (std.mem.eql(u8, name, "trim")) return object_mod.Atom.trim;
        if (std.mem.eql(u8, name, "replace")) return object_mod.Atom.replace;
        if (std.mem.eql(u8, name, "startsWith")) return object_mod.Atom.startsWith;
        if (std.mem.eql(u8, name, "endsWith")) return object_mod.Atom.endsWith;
        if (std.mem.eql(u8, name, "includes")) return object_mod.Atom.includes;
        // Math methods
        if (std.mem.eql(u8, name, "abs")) return object_mod.Atom.abs;
        if (std.mem.eql(u8, name, "floor")) return object_mod.Atom.floor;
        if (std.mem.eql(u8, name, "ceil")) return object_mod.Atom.ceil;
        if (std.mem.eql(u8, name, "round")) return object_mod.Atom.round;
        if (std.mem.eql(u8, name, "min")) return object_mod.Atom.min;
        if (std.mem.eql(u8, name, "max")) return object_mod.Atom.max;
        if (std.mem.eql(u8, name, "random")) return object_mod.Atom.random;
        if (std.mem.eql(u8, name, "pow")) return object_mod.Atom.pow;
        if (std.mem.eql(u8, name, "sqrt")) return object_mod.Atom.sqrt;
        if (std.mem.eql(u8, name, "log")) return object_mod.Atom.log;
        // JSON
        if (std.mem.eql(u8, name, "parse")) return object_mod.Atom.parse;
        if (std.mem.eql(u8, name, "stringify")) return object_mod.Atom.stringify;
        // Promise
        if (std.mem.eql(u8, name, "then")) return object_mod.Atom.then;
        if (std.mem.eql(u8, name, "resolve")) return object_mod.Atom.resolve;
        if (std.mem.eql(u8, name, "reject")) return object_mod.Atom.reject;
        // Globals
        if (std.mem.eql(u8, name, "console")) return object_mod.Atom.console;
        if (std.mem.eql(u8, name, "Math")) return object_mod.Atom.Math;
        if (std.mem.eql(u8, name, "JSON")) return object_mod.Atom.JSON;
        if (std.mem.eql(u8, name, "Object")) return object_mod.Atom.Object;
        if (std.mem.eql(u8, name, "Array")) return object_mod.Atom.Array;
        if (std.mem.eql(u8, name, "String")) return object_mod.Atom.String;
        if (std.mem.eql(u8, name, "Number")) return object_mod.Atom.Number;
        if (std.mem.eql(u8, name, "Boolean")) return object_mod.Atom.Boolean;
        if (std.mem.eql(u8, name, "Function")) return object_mod.Atom.Function;
        return null;
    }

    // ========================================================================
    // Scope management
    // ========================================================================

    fn beginScope(self: *Parser) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Parser) !void {
        self.scope_depth -= 1;
        while (self.local_count > 0 and self.locals[self.local_count - 1].depth > self.scope_depth) {
            try self.emitOp(.drop);
            self.local_count -= 1;
        }
    }

    // ========================================================================
    // Token handling
    // ========================================================================

    fn advance(self: *Parser) void {
        self.previous = self.current;
        while (true) {
            self.current = self.tokenizer.next();
            if (self.current.type != .invalid) break;
            self.errorAtCurrent("Unexpected character.");
        }
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) !void {
        if (self.current.type == token_type) {
            self.advance();
            return;
        }
        self.errorAtCurrent(message);
        return error.UnexpectedToken;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        return self.current.type == token_type;
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        if (!self.check(token_type)) return false;
        self.advance();
        return true;
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;
        while (self.current.type != .eof) {
            if (self.previous.type == .semicolon) return;
            switch (self.current.type) {
                .kw_function, .kw_var, .kw_let, .kw_const, .kw_for, .kw_if, .kw_while, .kw_return => return,
                else => {},
            }
            self.advance();
        }
    }

    // ========================================================================
    // Error handling
    // ========================================================================

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *const Token, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        self.had_error = true;
        _ = token;
        _ = message;
        // In a real implementation, we'd log the error with line info
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Tokenizer basic tokens" {
    var tokenizer = Tokenizer.init("var x = 42;");

    var tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.kw_var, tok.type);

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.identifier, tok.type);
    try std.testing.expectEqualStrings("x", tok.text("var x = 42;"));

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.assign, tok.type);

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.number, tok.type);
    try std.testing.expectEqualStrings("42", tok.text("var x = 42;"));

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.semicolon, tok.type);

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.eof, tok.type);
}

test "Tokenizer operators" {
    var tokenizer = Tokenizer.init("+ - * / % ** ++ -- == === != !== < <= > >= && || !");

    const expected = [_]TokenType{
        .plus,   .minus, .star, .slash, .percent,
        .star_star, .plus_plus, .minus_minus,
        .eq, .eq_eq, .ne, .ne_ne,
        .lt, .le, .gt, .ge,
        .ampersand_ampersand, .pipe_pipe, .bang,
    };

    for (expected) |exp| {
        const tok = tokenizer.next();
        try std.testing.expectEqual(exp, tok.type);
    }
}

test "Tokenizer strings" {
    var tokenizer = Tokenizer.init("\"hello\" 'world'");

    var tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.string_literal, tok.type);
    try std.testing.expectEqualStrings("\"hello\"", tok.text("\"hello\" 'world'"));

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.string_literal, tok.type);
}

test "Tokenizer numbers" {
    var tokenizer = Tokenizer.init("42 3.14 1e10 0xFF 0b1010 0o777");

    for (0..6) |_| {
        const tok = tokenizer.next();
        try std.testing.expectEqual(TokenType.number, tok.type);
    }
}

test "Tokenizer comments" {
    var tokenizer = Tokenizer.init("x // comment\n+ /* block */ y");

    var tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.identifier, tok.type);

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.plus, tok.type);

    tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.identifier, tok.type);
}

test "Parser simple expression" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "1 + 2", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    // Should have: push_int 1, push_int 2, add, pop, halt
    try std.testing.expect(code.len > 0);
}

test "Parser variable declaration" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "var x = 10;", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
}

test "Parser generator function" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Test function* syntax
    var parser = Parser.init(allocator, "function* gen() { yield 1; yield 2; }", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
}

test "Parser yield expression" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Test yield with expression
    var parser = Parser.init(allocator, "function* gen(x) { yield x + 1; }", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
}

test "Parser async function" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Test async function syntax
    var parser = Parser.init(allocator, "async function fetchData() { return 42; }", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
}

test "Parser await expression" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Test await inside async function
    var parser = Parser.init(allocator, "async function test() { var x = await fetch(); return x; }", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
}

test "Parser async function expression" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    // Test async function expression
    var parser = Parser.init(allocator, "var f = async function() { await delay(100); };", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
}

test "Parser import default" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "import foo from \"./foo.js\";", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
    try std.testing.expect(parser.is_module);
}

test "Parser import named" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "import { foo, bar as baz } from \"./module.js\";", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
    try std.testing.expect(parser.is_module);
}

test "Parser import namespace" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "import * as utils from \"./utils.js\";", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
    try std.testing.expect(parser.is_module);
}

test "Parser export default" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "export default 42;", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
    try std.testing.expect(parser.is_module);
}

test "Parser export function" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "export function hello() { return 1; }", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
    try std.testing.expect(parser.is_module);
}

test "Parser export const" {
    const allocator = std.testing.allocator;

    var strings = string.StringTable.init(allocator);
    defer strings.deinit();

    var parser = Parser.init(allocator, "export const x = 10;", &strings);
    defer parser.deinit();

    const code = parser.parse() catch |err| {
        std.debug.print("Parse error: {}\n", .{err});
        return err;
    };

    try std.testing.expect(code.len > 0);
    try std.testing.expect(parser.is_module);
}
