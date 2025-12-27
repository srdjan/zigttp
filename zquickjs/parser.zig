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
        if (self.match(.kw_var) or self.match(.kw_let) or self.match(.kw_const)) {
            try self.varDeclaration(self.previous.type == .kw_const);
        } else if (self.match(.kw_function)) {
            try self.functionDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) self.synchronize();
    }

    fn varDeclaration(self: *Parser, is_const: bool) Error!void {
        const name = try self.parseVariable("Expected variable name.");

        if (self.match(.assign)) {
            try self.expression();
        } else {
            try self.emitOp(.push_undefined);
        }

        try self.defineVariable(name, is_const);
        _ = self.match(.semicolon);
    }

    fn functionDeclaration(self: *Parser) Error!void {
        const name = try self.parseVariable("Expected function name.");
        try self.compileFunction(name);
        try self.defineVariable(name, false);
    }

    /// Parse and compile a function, pushing its value onto the stack
    fn compileFunction(self: *Parser, name: []const u8) Error!void {
        try self.consume(.lparen, "Expected '(' after function name.");

        // Parse parameters
        var params: [255][]const u8 = undefined;
        var param_count: u8 = 0;
        if (!self.check(.rparen)) {
            while (true) {
                if (param_count >= 255) {
                    self.errorAtCurrent("Cannot have more than 255 parameters.");
                    return error.TooManyLocals;
                }
                try self.consume(.identifier, "Expected parameter name.");
                params[param_count] = self.previous.text(self.tokenizer.source);
                param_count += 1;
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
        for (0..param_count) |i| {
            self.locals[self.local_count] = .{
                .name = params[i],
                .depth = 1,
                .is_const = false,
            };
            self.local_count += 1;
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
        } else if (self.match(.kw_return)) {
            try self.returnStatement();
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

        // Initializer
        if (self.match(.semicolon)) {
            // No initializer
        } else if (self.match(.kw_var) or self.match(.kw_let)) {
            try self.varDeclaration(false);
        } else {
            try self.expressionStatement();
        }

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

    fn returnStatement(self: *Parser) !void {
        if (self.check(.semicolon) or self.check(.rbrace) or self.check(.eof)) {
            try self.emitOp(.push_undefined);
        } else {
            try self.expression();
        }
        _ = self.match(.semicolon);
        try self.emitOp(.ret);
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
        const prefix_rule = getRule(self.previous.type).prefix;
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
            // Float goes in constant pool (TODO: allocate Float64Box)
            const const_idx = try self.addConstant(value.JSValue.fromInt(0)); // Placeholder
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

    fn identifier(self: *Parser, can_assign: bool) !void {
        const name = self.previous.text(self.tokenizer.source);

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
        try self.expression();
        try self.consume(.rparen, "Expected ')' after expression.");
    }

    /// Parse array literal: [elem1, elem2, ...]
    fn arrayLiteral(self: *Parser, _: bool) !void {
        // Create empty array first
        try self.emitOp(.new_array);
        try self.emitU16(0);

        var index: u16 = 0;
        if (!self.check(.rbracket)) {
            while (true) {
                // Stack: [array]
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
        const length_atom = @intFromEnum(@import("object.zig").Atom.length);
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

    /// Parse 'this' keyword
    fn thisExpr(self: *Parser, _: bool) !void {
        try self.emitOp(.push_this);
    }

    /// Parse function expression: function() {} or function name() {}
    fn functionExpr(self: *Parser, _: bool) !void {
        // Optional name for named function expressions
        var name: []const u8 = "";
        if (self.match(.identifier)) {
            name = self.previous.text(self.tokenizer.source);
        }
        try self.compileFunction(name);
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
            .question => .{ .prefix = null, .infix = ternary, .precedence = .ternary },
            .minus => .{ .prefix = unary, .infix = binary, .precedence = .term },
            .plus => .{ .prefix = null, .infix = binary, .precedence = .term },
            .slash, .star, .percent => .{ .prefix = null, .infix = binary, .precedence = .factor },
            .bang, .tilde => .{ .prefix = unary, .infix = null, .precedence = .none },
            .kw_typeof => .{ .prefix = unary, .infix = null, .precedence = .none },
            .eq, .ne, .eq_eq, .ne_ne => .{ .prefix = null, .infix = binary, .precedence = .equality },
            .lt, .le, .gt, .ge => .{ .prefix = null, .infix = binary, .precedence = .comparison },
            .ampersand, .pipe, .caret => .{ .prefix = null, .infix = binary, .precedence = .term },
            .lt_lt, .gt_gt, .gt_gt_gt => .{ .prefix = null, .infix = binary, .precedence = .term },
            .ampersand_ampersand => .{ .prefix = null, .infix = andOp, .precedence = .and_prec },
            .pipe_pipe => .{ .prefix = null, .infix = orOp, .precedence = .or_prec },
            .number => .{ .prefix = number, .infix = null, .precedence = .none },
            .string_literal => .{ .prefix = stringLiteral, .infix = null, .precedence = .none },
            .identifier => .{ .prefix = identifier, .infix = null, .precedence = .none },
            .kw_this => .{ .prefix = thisExpr, .infix = null, .precedence = .none },
            .kw_function => .{ .prefix = functionExpr, .infix = null, .precedence = .none },
            .@"true", .@"false", .@"null", .undefined => .{ .prefix = literal, .infix = null, .precedence = .none },
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
