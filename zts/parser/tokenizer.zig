//! Unified Tokenizer for JavaScript and JSX
//!
//! Single-pass tokenizer with JSX mode support, column tracking,
//! and lookahead for arrow function detection.

const std = @import("std");
const token = @import("token.zig");

pub const Token = token.Token;
pub const TokenType = token.TokenType;
pub const SourceLocation = token.SourceLocation;
pub const lookupKeyword = token.lookupKeyword;

/// Tokenizer state
pub const Tokenizer = struct {
    source: []const u8,
    pos: u32,
    line: u32,
    line_start: u32,

    /// JSX parsing mode
    jsx_mode: bool,

    /// Stack for tracking JSX depth (for nested elements)
    jsx_depth: u16,

    /// Whether we just saw a token that could precede a regex
    can_be_regex: bool,

    /// Template literal depth
    template_depth: u8,

    /// Cached token for lookahead
    current: Token,
    has_current: bool,

    pub fn init(source: []const u8) Tokenizer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .line_start = 0,
            .jsx_mode = false,
            .jsx_depth = 0,
            .can_be_regex = true,
            .template_depth = 0,
            .current = undefined,
            .has_current = false,
        };
    }

    /// Get the current column number (1-indexed)
    pub fn column(self: *const Tokenizer) u16 {
        return @intCast(self.pos - self.line_start + 1);
    }

    /// Get the next token
    pub fn next(self: *Tokenizer) Token {
        self.skipWhitespaceAndComments();

        if (self.pos >= self.source.len) {
            return self.makeToken(.eof, 0);
        }

        const start = self.pos;
        const start_col = self.column();
        const start_line = self.line;
        const c = self.advance();

        const tok = switch (c) {
            '(' => self.tok1(start, start_col, start_line, .lparen),
            ')' => self.tok1(start, start_col, start_line, .rparen),
            '[' => self.tok1(start, start_col, start_line, .lbracket),
            ']' => self.tok1(start, start_col, start_line, .rbracket),
            ',' => self.tok1(start, start_col, start_line, .comma),
            ';' => self.tok1(start, start_col, start_line, .semicolon),
            ':' => self.tok1(start, start_col, start_line, .colon),
            '~' => self.tok1(start, start_col, start_line, .tilde),

            '{' => blk: {
                break :blk self.tok1(start, start_col, start_line, .lbrace);
            },
            '}' => blk: {
                if (self.template_depth > 0) {
                    break :blk self.scanTemplateMiddleOrTail(start, start_col, start_line);
                }
                break :blk self.tok1(start, start_col, start_line, .rbrace);
            },

            '?' => self.scanQuestion(start, start_col, start_line),
            '+' => self.scanPlus(start, start_col, start_line),
            '-' => self.scanMinus(start, start_col, start_line),
            '*' => self.scanStar(start, start_col, start_line),
            '/' => self.scanSlash(start, start_col, start_line),
            '%' => self.scanPercent(start, start_col, start_line),
            '=' => self.scanEquals(start, start_col, start_line),
            '!' => self.scanBang(start, start_col, start_line),
            '<' => self.scanLessThan(start, start_col, start_line),
            '>' => self.scanGreaterThan(start, start_col, start_line),
            '&' => self.scanAmpersand(start, start_col, start_line),
            '|' => self.scanPipe(start, start_col, start_line),
            '^' => self.scanCaret(start, start_col, start_line),
            '.' => self.scanDot(start, start_col, start_line),

            '"', '\'' => self.scanString(c, start, start_col, start_line),
            '`' => self.scanTemplateLiteral(start, start_col, start_line),
            '0'...'9' => self.scanNumber(start, start_col, start_line),

            else => blk: {
                if (isIdentifierStart(c)) {
                    break :blk self.scanIdentifier(start, start_col, start_line);
                }
                break :blk self.tok1(start, start_col, start_line, .invalid);
            },
        };

        self.updateRegexState(tok.type);
        return tok;
    }

    // --- Token constructors ---

    fn tok1(self: *const Tokenizer, start: u32, col: u16, line: u32, t: TokenType) Token {
        _ = self;
        return .{ .type = t, .start = start, .len = 1, .line = line, .column = col };
    }

    fn tok2(self: *const Tokenizer, start: u32, col: u16, line: u32, t: TokenType) Token {
        _ = self;
        return .{ .type = t, .start = start, .len = 2, .line = line, .column = col };
    }

    fn tok3(self: *const Tokenizer, start: u32, col: u16, line: u32, t: TokenType) Token {
        _ = self;
        return .{ .type = t, .start = start, .len = 3, .line = line, .column = col };
    }

    fn tok4(self: *const Tokenizer, start: u32, col: u16, line: u32, t: TokenType) Token {
        _ = self;
        return .{ .type = t, .start = start, .len = 4, .line = line, .column = col };
    }

    fn tokN(self: *const Tokenizer, start: u32, col: u16, line: u32, t: TokenType) Token {
        return .{ .type = t, .start = start, .len = @intCast(self.pos - start), .line = line, .column = col };
    }

    // --- Operator scanning ---

    fn scanQuestion(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('?')) {
            if (self.match('=')) return self.tok3(start, col, line, .question_question_assign);
            return self.tok2(start, col, line, .question_question);
        }
        if (self.match('.')) {
            if (self.pos < self.source.len and isDigit(self.source[self.pos])) {
                self.pos -= 1;
                return self.tok1(start, col, line, .question);
            }
            return self.tok2(start, col, line, .question_dot);
        }
        return self.tok1(start, col, line, .question);
    }

    fn scanPlus(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('+')) return self.tok2(start, col, line, .plus_plus);
        if (self.match('=')) return self.tok2(start, col, line, .plus_assign);
        return self.tok1(start, col, line, .plus);
    }

    fn scanMinus(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('-')) return self.tok2(start, col, line, .minus_minus);
        if (self.match('=')) return self.tok2(start, col, line, .minus_assign);
        return self.tok1(start, col, line, .minus);
    }

    fn scanStar(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('*')) {
            if (self.match('=')) return self.tok3(start, col, line, .star_star_assign);
            return self.tok2(start, col, line, .star_star);
        }
        if (self.match('=')) return self.tok2(start, col, line, .star_assign);
        return self.tok1(start, col, line, .star);
    }

    fn scanSlash(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('=')) return self.tok2(start, col, line, .slash_assign);
        if (self.can_be_regex) {
            if (self.scanRegexLiteral(start, col, line)) |tok| {
                return tok;
            }
        }
        // RegExp literals not supported - treat / as division when not a regex
        return self.tok1(start, col, line, .slash);
    }

    fn scanPercent(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('=')) return self.tok2(start, col, line, .percent_assign);
        return self.tok1(start, col, line, .percent);
    }

    fn scanEquals(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('=')) {
            if (self.match('=')) return self.tok3(start, col, line, .eq_eq);
            return self.tok2(start, col, line, .eq);
        }
        if (self.match('>')) return self.tok2(start, col, line, .arrow);
        return self.tok1(start, col, line, .assign);
    }

    fn scanBang(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('=')) {
            if (self.match('=')) return self.tok3(start, col, line, .ne_ne);
            return self.tok2(start, col, line, .ne);
        }
        return self.tok1(start, col, line, .bang);
    }

    fn scanLessThan(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('<')) {
            if (self.match('=')) return self.tok3(start, col, line, .lt_lt_assign);
            return self.tok2(start, col, line, .lt_lt);
        }
        if (self.match('=')) return self.tok2(start, col, line, .le);
        return self.tok1(start, col, line, .lt);
    }

    fn scanGreaterThan(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('>')) {
            if (self.match('>')) {
                if (self.match('=')) return self.tok4(start, col, line, .gt_gt_gt_assign);
                return self.tok3(start, col, line, .gt_gt_gt);
            }
            if (self.match('=')) return self.tok3(start, col, line, .gt_gt_assign);
            return self.tok2(start, col, line, .gt_gt);
        }
        if (self.match('=')) return self.tok2(start, col, line, .ge);
        return self.tok1(start, col, line, .gt);
    }

    fn scanRegexLiteral(self: *Tokenizer, start: u32, col: u16, line: u32) ?Token {
        const saved = self.saveState();
        var in_class = false;
        var escaped = false;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '\n' or c == '\r') {
                self.restoreState(saved);
                return null;
            }
            self.pos += 1;

            if (escaped) {
                escaped = false;
                continue;
            }
            if (c == '\\') {
                escaped = true;
                continue;
            }
            if (c == '[') {
                in_class = true;
                continue;
            }
            if (c == ']' and in_class) {
                in_class = false;
                continue;
            }
            if (c == '/' and !in_class) {
                // Optional flags
                while (self.pos < self.source.len) {
                    const f = self.source[self.pos];
                    if (std.ascii.isAlphabetic(f)) {
                        self.pos += 1;
                        continue;
                    }
                    break;
                }
                const len: u16 = @intCast(self.pos - start);
                return .{ .type = .regex_literal, .start = start, .len = len, .line = line, .column = col };
            }
        }

        self.restoreState(saved);
        return null;
    }

    fn scanAmpersand(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('&')) {
            if (self.match('=')) return self.tok3(start, col, line, .ampersand_ampersand_assign);
            return self.tok2(start, col, line, .ampersand_ampersand);
        }
        if (self.match('=')) return self.tok2(start, col, line, .ampersand_assign);
        return self.tok1(start, col, line, .ampersand);
    }

    fn scanPipe(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('|')) {
            if (self.match('=')) return self.tok3(start, col, line, .pipe_pipe_assign);
            return self.tok2(start, col, line, .pipe_pipe);
        }
        if (self.match('=')) return self.tok2(start, col, line, .pipe_assign);
        return self.tok1(start, col, line, .pipe);
    }

    fn scanCaret(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('=')) return self.tok2(start, col, line, .caret_assign);
        return self.tok1(start, col, line, .caret);
    }

    fn scanDot(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        if (self.match('.') and self.match('.')) return self.tok3(start, col, line, .spread);
        if (self.pos < self.source.len and isDigit(self.source[self.pos])) {
            return self.scanNumber(start, col, line);
        }
        return self.tok1(start, col, line, .dot);
    }

    // --- Literal scanning ---

    fn scanString(self: *Tokenizer, quote: u8, start: u32, col: u16, line: u32) Token {
        while (self.pos < self.source.len and self.source[self.pos] != quote) {
            if (self.source[self.pos] == '\\' and self.pos + 1 < self.source.len) {
                self.pos += 2;
            } else {
                if (self.source[self.pos] == '\n') {
                    self.line += 1;
                    self.line_start = self.pos + 1;
                }
                self.pos += 1;
            }
        }
        if (self.pos < self.source.len) self.pos += 1;
        return self.tokN(start, col, line, .string_literal);
    }

    fn scanTemplateLiteral(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '`') {
                self.pos += 1;
                return self.tokN(start, col, line, .template_literal);
            } else if (c == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                self.pos += 2;
                self.template_depth += 1;
                return self.tokN(start, col, line, .template_head);
            } else if (c == '\\' and self.pos + 1 < self.source.len) {
                self.pos += 2;
            } else {
                if (c == '\n') {
                    self.line += 1;
                    self.line_start = self.pos + 1;
                }
                self.pos += 1;
            }
        }
        return self.tokN(start, col, line, .template_literal);
    }

    fn scanTemplateMiddleOrTail(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '`') {
                self.pos += 1;
                self.template_depth -= 1;
                return self.tokN(start, col, line, .template_tail);
            } else if (c == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                self.pos += 2;
                return self.tokN(start, col, line, .template_middle);
            } else if (c == '\\' and self.pos + 1 < self.source.len) {
                self.pos += 2;
            } else {
                if (c == '\n') {
                    self.line += 1;
                    self.line_start = self.pos + 1;
                }
                self.pos += 1;
            }
        }
        self.template_depth -|= 1;
        return self.tokN(start, col, line, .template_tail);
    }

    fn scanNumber(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        // Handle hex, octal, binary
        if (start < self.source.len and self.source[start] == '0' and self.pos < self.source.len) {
            const next_char = self.source[self.pos];
            if (next_char == 'x' or next_char == 'X') {
                self.pos += 1;
                while (self.pos < self.source.len and isHexDigit(self.source[self.pos])) self.pos += 1;
                return self.tokN(start, col, line, .number);
            }
            if (next_char == 'b' or next_char == 'B') {
                self.pos += 1;
                while (self.pos < self.source.len and (self.source[self.pos] == '0' or self.source[self.pos] == '1')) self.pos += 1;
                return self.tokN(start, col, line, .number);
            }
            if (next_char == 'o' or next_char == 'O') {
                self.pos += 1;
                while (self.pos < self.source.len and self.source[self.pos] >= '0' and self.source[self.pos] <= '7') self.pos += 1;
                return self.tokN(start, col, line, .number);
            }
        }

        while (self.pos < self.source.len and isDigit(self.source[self.pos])) self.pos += 1;

        if (self.pos < self.source.len and self.source[self.pos] == '.') {
            if (self.pos + 1 < self.source.len and isDigit(self.source[self.pos + 1])) {
                self.pos += 1;
                while (self.pos < self.source.len and isDigit(self.source[self.pos])) self.pos += 1;
            }
        }

        if (self.pos < self.source.len and (self.source[self.pos] == 'e' or self.source[self.pos] == 'E')) {
            self.pos += 1;
            if (self.pos < self.source.len and (self.source[self.pos] == '+' or self.source[self.pos] == '-')) self.pos += 1;
            while (self.pos < self.source.len and isDigit(self.source[self.pos])) self.pos += 1;
        }

        return self.tokN(start, col, line, .number);
    }

    fn scanIdentifier(self: *Tokenizer, start: u32, col: u16, line: u32) Token {
        while (self.pos < self.source.len and isIdentifierChar(self.source[self.pos])) self.pos += 1;
        const text = self.source[start..self.pos];
        const token_type = lookupKeyword(text) orelse .identifier;
        return self.tokN(start, col, line, token_type);
    }

    // --- Utility ---

    fn advance(self: *Tokenizer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        return c;
    }

    fn match(self: *Tokenizer, expected: u8) bool {
        if (self.pos >= self.source.len) return false;
        if (self.source[self.pos] != expected) return false;
        self.pos += 1;
        return true;
    }

    fn makeToken(self: *const Tokenizer, token_type: TokenType, len: u16) Token {
        return .{
            .type = token_type,
            .start = self.pos - len,
            .len = len,
            .line = self.line,
            .column = @intCast(@max(1, @as(i32, @intCast(self.pos)) - @as(i32, @intCast(self.line_start)) - @as(i32, len) + 1)),
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
                            self.pos += 2;
                            while (self.pos < self.source.len and self.source[self.pos] != '\n') self.pos += 1;
                        } else if (self.source[self.pos + 1] == '*') {
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
                        } else return;
                    } else return;
                },
                else => return,
            }
        }
    }

    fn updateRegexState(self: *Tokenizer, tok_type: TokenType) void {
        self.can_be_regex = switch (tok_type) {
            .lparen, .lbracket, .lbrace, .comma, .semicolon, .colon, .question,
            .assign, .plus_assign, .minus_assign, .star_assign, .slash_assign,
            .percent_assign, .ampersand_assign, .pipe_assign, .caret_assign,
            .lt_lt_assign, .gt_gt_assign, .gt_gt_gt_assign, .star_star_assign,
            .ampersand_ampersand_assign, .pipe_pipe_assign, .question_question_assign,
            .plus, .minus, .star, .slash, .percent, .star_star,
            .lt, .le, .gt, .ge, .eq, .ne, .eq_eq, .ne_ne,
            .ampersand, .pipe, .caret, .ampersand_ampersand, .pipe_pipe, .question_question,
            .bang, .tilde, .kw_return, .kw_throw, .kw_new, .kw_typeof,
            .kw_void, .kw_delete, .kw_in, .kw_instanceof, .kw_case, .arrow,
            => true,
            else => false,
        };
    }

    /// Enable JSX mode
    pub fn enableJsx(self: *Tokenizer) void {
        self.jsx_mode = true;
    }

    /// Save state for lookahead
    pub fn saveState(self: *const Tokenizer) TokenizerState {
        return .{
            .pos = self.pos,
            .line = self.line,
            .line_start = self.line_start,
            .can_be_regex = self.can_be_regex,
            .template_depth = self.template_depth,
        };
    }

    /// Restore state
    pub fn restoreState(self: *Tokenizer, state: TokenizerState) void {
        self.pos = state.pos;
        self.line = state.line;
        self.line_start = state.line_start;
        self.can_be_regex = state.can_be_regex;
        self.template_depth = state.template_depth;
    }
};

pub const TokenizerState = struct {
    pos: u32,
    line: u32,
    line_start: u32,
    can_be_regex: bool,
    template_depth: u8,
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

// --- Tests ---

test "basic tokenization" {
    var tokenizer = Tokenizer.init("let x = 42;");

    const let_tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.kw_let, let_tok.type);
    try std.testing.expectEqual(@as(u32, 1), let_tok.line);

    const x_tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.identifier, x_tok.type);
    try std.testing.expectEqualStrings("x", x_tok.text("let x = 42;"));

    const eq_tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.assign, eq_tok.type);

    const num_tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.number, num_tok.type);

    const semi_tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.semicolon, semi_tok.type);

    const eof_tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.eof, eof_tok.type);
}

test "template literals" {
    var tokenizer = Tokenizer.init("`hello ${name}!`");

    const head = tokenizer.next();
    try std.testing.expectEqual(TokenType.template_head, head.type);

    const name_tok = tokenizer.next();
    try std.testing.expectEqual(TokenType.identifier, name_tok.type);

    const tail = tokenizer.next();
    try std.testing.expectEqual(TokenType.template_tail, tail.type);
}

test "arrow functions" {
    var tokenizer = Tokenizer.init("(x) => x * 2");

    try std.testing.expectEqual(TokenType.lparen, tokenizer.next().type);
    try std.testing.expectEqual(TokenType.identifier, tokenizer.next().type);
    try std.testing.expectEqual(TokenType.rparen, tokenizer.next().type);
    try std.testing.expectEqual(TokenType.arrow, tokenizer.next().type);
    try std.testing.expectEqual(TokenType.identifier, tokenizer.next().type);
    try std.testing.expectEqual(TokenType.star, tokenizer.next().type);
    try std.testing.expectEqual(TokenType.number, tokenizer.next().type);
}

test "column tracking" {
    var tokenizer = Tokenizer.init("let x");

    const let_tok = tokenizer.next();
    try std.testing.expectEqual(@as(u16, 1), let_tok.column);

    const x_tok = tokenizer.next();
    try std.testing.expectEqual(@as(u16, 5), x_tok.column);
}

test "multiline column tracking" {
    var tokenizer = Tokenizer.init("a\nb");

    const a_tok = tokenizer.next();
    try std.testing.expectEqual(@as(u32, 1), a_tok.line);
    try std.testing.expectEqual(@as(u16, 1), a_tok.column);

    const b_tok = tokenizer.next();
    try std.testing.expectEqual(@as(u32, 2), b_tok.line);
    try std.testing.expectEqual(@as(u16, 1), b_tok.column);
}

test "state save and restore" {
    var tokenizer = Tokenizer.init("a b c");

    _ = tokenizer.next(); // a
    const state = tokenizer.saveState();

    _ = tokenizer.next(); // b
    _ = tokenizer.next(); // c

    tokenizer.restoreState(state);
    const b_tok = tokenizer.next();
    try std.testing.expectEqualStrings("b", b_tok.text("a b c"));
}
