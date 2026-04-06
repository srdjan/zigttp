//! Compile-Time Expression Evaluator
//!
//! Evaluates comptime(<expr>) expressions during TypeScript stripping.
//! Uses a Pratt parser to handle operator precedence correctly.
//!
//! Supported:
//! - Literals: number, string, boolean, null, undefined, NaN, Infinity
//! - Unary: + - ! ~
//! - Binary: + - * / % ** | & ^ << >> >>> == != === !== < <= > >= && || ??
//! - Ternary: cond ? a : b
//! - Grouping: (expr)
//! - Arrays: [1, 2, 3]
//! - Objects: { a: 1, b: 2 }
//! - Math: Math.PI, Math.abs(), etc.
//!
//! Disallowed:
//! - Variables (except whitelisted globals)
//! - Function calls (except whitelisted)
//! - new, this, eval, assignments, loops

const std = @import("std");

// ============================================================================
// Error Types
// ============================================================================

pub const ComptimeError = error{
    UnsupportedOp,
    UnknownIdentifier,
    CallNotAllowed,
    SyntaxError,
    DepthExceeded,
    ExpressionTooLong,
    TypeMismatch,
    DivisionByZero,
    UnclosedString,
    UnclosedParen,
    UnclosedBracket,
    UnclosedBrace,
    UnexpectedToken,
    UnexpectedEnd,
    OutOfMemory,
    InvalidNumber,
    InvalidEscape,
};

// ============================================================================
// Value Type
// ============================================================================

pub const ComptimeValue = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    null_val: void,
    undefined_val: void,
    nan_val: void,
    infinity: Infinity,
    array: []const ComptimeValue,
    object: []const ObjectProperty,

    pub const Infinity = struct { negative: bool };

    pub const ObjectProperty = struct {
        key: []const u8,
        value: ComptimeValue,
    };

    /// Convert to f64 for numeric operations
    pub fn toNumber(self: ComptimeValue) ?f64 {
        return switch (self) {
            .number => |n| n,
            .boolean => |b| if (b) 1.0 else 0.0,
            .null_val => 0.0,
            .undefined_val => std.math.nan(f64),
            .nan_val => std.math.nan(f64),
            .infinity => |i| if (i.negative) -std.math.inf(f64) else std.math.inf(f64),
            .string => |s| std.fmt.parseFloat(f64, s) catch null,
            .array, .object => null,
        };
    }

    /// Convert to boolean for logical operations (JS truthiness)
    pub fn toBool(self: ComptimeValue) bool {
        return switch (self) {
            .boolean => |b| b,
            .number => |n| n != 0.0 and !std.math.isNan(n),
            .string => |s| s.len > 0,
            .null_val, .undefined_val => false,
            .nan_val => false,
            .infinity => true,
            .array, .object => true,
        };
    }

    /// Check if value is nullish (null or undefined)
    pub fn isNullish(self: ComptimeValue) bool {
        return switch (self) {
            .null_val, .undefined_val => true,
            else => false,
        };
    }

    /// Strict equality (===)
    pub fn strictEquals(self: ComptimeValue, other: ComptimeValue) bool {
        const self_tag = @intFromEnum(self);
        const other_tag = @intFromEnum(other);
        if (self_tag != other_tag) return false;

        return switch (self) {
            .number => |n| {
                const o = other.number;
                // NaN !== NaN
                if (std.math.isNan(n) or std.math.isNan(o)) return false;
                return n == o;
            },
            .string => |s| std.mem.eql(u8, s, other.string),
            .boolean => |b| b == other.boolean,
            .null_val, .undefined_val => true,
            .nan_val => false, // NaN !== NaN
            .infinity => |i| i.negative == other.infinity.negative,
            .array, .object => false, // Reference equality not supported
        };
    }

    /// Loose equality (==)
    pub fn looseEquals(self: ComptimeValue, other: ComptimeValue) bool {
        // Same type: use strict
        const self_tag = @intFromEnum(self);
        const other_tag = @intFromEnum(other);
        if (self_tag == other_tag) return self.strictEquals(other);

        // null == undefined
        if (self.isNullish() and other.isNullish()) return true;

        // Number comparisons
        const self_num = self.toNumber();
        const other_num = other.toNumber();
        if (self_num != null and other_num != null) {
            const sn = self_num.?;
            const on = other_num.?;
            if (std.math.isNan(sn) or std.math.isNan(on)) return false;
            return sn == on;
        }

        return false;
    }

    /// Free all allocated memory in this value
    pub fn deinit(self: ComptimeValue, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => |s| allocator.free(s),
            .array => |arr| {
                for (arr) |elem| {
                    elem.deinit(allocator);
                }
                allocator.free(arr);
            },
            .object => |props| {
                for (props) |prop| {
                    allocator.free(prop.key);
                    prop.value.deinit(allocator);
                }
                allocator.free(props);
            },
            else => {},
        }
    }
};

// ============================================================================
// Operator Precedence
// ============================================================================

const Precedence = enum(u8) {
    none = 0,
    ternary = 1, // ?:
    nullish = 2, // ??
    or_op = 3, // ||
    and_op = 4, // &&
    bit_or = 5, // |
    bit_xor = 6, // ^
    bit_and = 7, // &
    equality = 8, // == != === !==
    comparison = 9, // < > <= >=
    shift = 10, // << >> >>>
    additive = 11, // + -
    multiplicative = 12, // * / %
    exponent = 13, // **
    unary = 14, // ! ~ - +
    call = 15, // () .
    primary = 16,
};

// ============================================================================
// Evaluator
// ============================================================================

pub const ComptimeEvaluator = struct {
    source: []const u8,
    pos: usize,
    allocator: std.mem.Allocator,

    // Position tracking for errors
    start_line: u32,
    start_col: u32,
    line: u32,
    col: u32,

    // Performance guards
    max_depth: u16 = 64,
    current_depth: u16 = 0,
    max_expr_len: usize = 8192,

    // Environment for Env.* lookups
    env: ?*const std.StringHashMap([]const u8) = null,

    // Build metadata
    build_time: ?[]const u8 = null,
    git_commit: ?[]const u8 = null,
    version: ?[]const u8 = null,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8, start_line: u32, start_col: u32) Self {
        return .{
            .source = source,
            .pos = 0,
            .allocator = allocator,
            .start_line = start_line,
            .start_col = start_col,
            .line = start_line,
            .col = start_col,
        };
    }

    /// Main entry point: evaluate the expression
    pub fn evaluate(self: *Self) ComptimeError!ComptimeValue {
        if (self.source.len > self.max_expr_len) {
            return ComptimeError.ExpressionTooLong;
        }

        self.skipWhitespace();
        const result = try self.parseExpression(.none);
        self.skipWhitespace();

        // Should have consumed all input
        if (self.pos < self.source.len) {
            return ComptimeError.UnexpectedToken;
        }

        return result;
    }

    // ========================================================================
    // Pratt Parser Core
    // ========================================================================

    fn parseExpression(self: *Self, min_prec: Precedence) ComptimeError!ComptimeValue {
        self.current_depth += 1;
        defer self.current_depth -= 1;

        if (self.current_depth > self.max_depth) {
            return ComptimeError.DepthExceeded;
        }

        // Parse prefix (primary or unary)
        var left = try self.parsePrimary();

        // Parse infix operators
        while (true) {
            self.skipWhitespace();
            if (self.pos >= self.source.len) break;

            const op_prec = self.getInfixPrecedence();
            if (@intFromEnum(op_prec) <= @intFromEnum(min_prec)) break;

            left = try self.parseInfix(left, op_prec);
        }

        return left;
    }

    fn parsePrimary(self: *Self) ComptimeError!ComptimeValue {
        self.skipWhitespace();
        if (self.pos >= self.source.len) return ComptimeError.UnexpectedEnd;

        const c = self.source[self.pos];

        // Unary operators
        if (c == '!' or c == '~' or c == '-' or c == '+') {
            return self.parseUnary();
        }

        // Grouping
        if (c == '(') {
            return self.parseGrouping();
        }

        // Array literal
        if (c == '[') {
            return self.parseArray();
        }

        // Object literal
        if (c == '{') {
            return self.parseObject();
        }

        // String literal
        if (c == '"' or c == '\'') {
            return self.parseString(c);
        }

        // Template literal (simple case only)
        if (c == '`') {
            return self.parseTemplateLiteral();
        }

        // Number literal
        if (isDigit(c) or (c == '.' and self.pos + 1 < self.source.len and isDigit(self.source[self.pos + 1]))) {
            return self.parseNumber();
        }

        // Identifier or keyword
        if (isIdentifierStart(c)) {
            return self.parseIdentifier();
        }

        return ComptimeError.UnexpectedToken;
    }

    fn parseUnary(self: *Self) ComptimeError!ComptimeValue {
        const op = self.source[self.pos];
        self.advance();
        self.skipWhitespace();

        const operand = try self.parseExpression(.unary);

        return switch (op) {
            '!' => .{ .boolean = !operand.toBool() },
            '~' => blk: {
                const n = operand.toNumber() orelse return ComptimeError.TypeMismatch;
                if (std.math.isNan(n) or std.math.isInf(n)) {
                    break :blk .{ .number = -1 };
                }
                const i: i32 = @intFromFloat(@trunc(n));
                break :blk .{ .number = @floatFromInt(~i) };
            },
            '-' => blk: {
                const n = operand.toNumber() orelse return ComptimeError.TypeMismatch;
                break :blk .{ .number = -n };
            },
            '+' => blk: {
                const n = operand.toNumber() orelse return ComptimeError.TypeMismatch;
                break :blk .{ .number = n };
            },
            else => unreachable,
        };
    }

    fn parseGrouping(self: *Self) ComptimeError!ComptimeValue {
        self.expect('(') catch return ComptimeError.SyntaxError;
        self.skipWhitespace();
        const inner = try self.parseExpression(.none);
        self.skipWhitespace();
        self.expect(')') catch return ComptimeError.UnclosedParen;
        return inner;
    }

    fn parseInfix(self: *Self, left: ComptimeValue, prec: Precedence) ComptimeError!ComptimeValue {
        self.skipWhitespace();

        // Ternary operator
        if (self.peek() == '?') {
            if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '?') {
                // Nullish coalescing ??
                self.advance();
                self.advance();
                self.skipWhitespace();
                const right = try self.parseExpression(.nullish);
                if (left.isNullish()) {
                    return right;
                } else {
                    right.deinit(self.allocator);
                    return left;
                }
            }
            // Ternary ?:
            self.advance();
            self.skipWhitespace();
            const then_val = try self.parseExpression(.none);
            self.skipWhitespace();
            self.expect(':') catch return ComptimeError.SyntaxError;
            self.skipWhitespace();
            const else_val = try self.parseExpression(.ternary);
            if (left.toBool()) {
                else_val.deinit(self.allocator);
                return then_val;
            } else {
                then_val.deinit(self.allocator);
                return else_val;
            }
        }

        // Two-char operators
        if (self.pos + 1 < self.source.len) {
            const two = self.source[self.pos .. self.pos + 2];

            if (std.mem.eql(u8, two, "||")) {
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(.or_op);
                if (left.toBool()) {
                    right.deinit(self.allocator);
                    return left;
                }
                return right;
            }
            if (std.mem.eql(u8, two, "&&")) {
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(.and_op);
                if (!left.toBool()) {
                    right.deinit(self.allocator);
                    return left;
                }
                return right;
            }
            if (std.mem.eql(u8, two, "==")) {
                if (self.pos + 2 < self.source.len and self.source[self.pos + 2] == '=') {
                    self.pos += 3; // ===
                    self.skipWhitespace();
                    const right = try self.parseExpression(prec);
                    return .{ .boolean = left.strictEquals(right) };
                }
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(prec);
                return .{ .boolean = left.looseEquals(right) };
            }
            if (std.mem.eql(u8, two, "!=")) {
                if (self.pos + 2 < self.source.len and self.source[self.pos + 2] == '=') {
                    self.pos += 3; // !==
                    self.skipWhitespace();
                    const right = try self.parseExpression(prec);
                    return .{ .boolean = !left.strictEquals(right) };
                }
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(prec);
                return .{ .boolean = !left.looseEquals(right) };
            }
            if (std.mem.eql(u8, two, "<=")) {
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(prec);
                const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
                const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
                return .{ .boolean = ln <= rn };
            }
            if (std.mem.eql(u8, two, ">=")) {
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(prec);
                const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
                const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
                return .{ .boolean = ln >= rn };
            }
            if (std.mem.eql(u8, two, "<<")) {
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(prec);
                return self.bitwiseShift(left, right, .left);
            }
            if (std.mem.eql(u8, two, ">>")) {
                if (self.pos + 2 < self.source.len and self.source[self.pos + 2] == '>') {
                    self.pos += 3; // >>>
                    self.skipWhitespace();
                    const right = try self.parseExpression(prec);
                    return self.bitwiseShift(left, right, .unsigned_right);
                }
                self.pos += 2;
                self.skipWhitespace();
                const right = try self.parseExpression(prec);
                return self.bitwiseShift(left, right, .right);
            }
            if (std.mem.eql(u8, two, "**")) {
                self.pos += 2;
                self.skipWhitespace();
                // Right-associative
                const right = try self.parseExpression(@enumFromInt(@intFromEnum(prec) - 1));
                const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
                const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
                return .{ .number = std.math.pow(f64, ln, rn) };
            }
        }

        // Member access for strings
        if (self.source[self.pos] == '.') {
            self.advance();
            self.skipWhitespace();
            return self.handleMemberAccess(left);
        }

        // Single-char operators
        const op = self.source[self.pos];
        self.advance();
        self.skipWhitespace();
        const right = try self.parseExpression(prec);

        return switch (op) {
            '+' => self.add(left, right),
            '-' => self.subtract(left, right),
            '*' => self.multiply(left, right),
            '/' => self.divide(left, right),
            '%' => self.modulo(left, right),
            '|' => self.bitwiseOr(left, right),
            '&' => self.bitwiseAnd(left, right),
            '^' => self.bitwiseXor(left, right),
            '<' => blk: {
                const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
                const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
                break :blk .{ .boolean = ln < rn };
            },
            '>' => blk: {
                const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
                const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
                break :blk .{ .boolean = ln > rn };
            },
            else => ComptimeError.UnsupportedOp,
        };
    }

    fn getInfixPrecedence(self: *Self) Precedence {
        if (self.pos >= self.source.len) return .none;

        const c = self.source[self.pos];

        // Check two-char operators first
        if (self.pos + 1 < self.source.len) {
            const two = self.source[self.pos .. self.pos + 2];
            if (std.mem.eql(u8, two, "??")) return .nullish;
            if (std.mem.eql(u8, two, "||")) return .or_op;
            if (std.mem.eql(u8, two, "&&")) return .and_op;
            if (std.mem.eql(u8, two, "==") or std.mem.eql(u8, two, "!=")) return .equality;
            if (std.mem.eql(u8, two, "<=") or std.mem.eql(u8, two, ">=")) return .comparison;
            if (std.mem.eql(u8, two, "<<") or std.mem.eql(u8, two, ">>")) return .shift;
            if (std.mem.eql(u8, two, "**")) return .exponent;
        }

        return switch (c) {
            '?' => .ternary,
            '|' => .bit_or,
            '^' => .bit_xor,
            '&' => .bit_and,
            '<', '>' => .comparison,
            '+', '-' => .additive,
            '*', '/', '%' => .multiplicative,
            '.' => .call,
            '(' => .call,
            else => .none,
        };
    }

    // ========================================================================
    // Literal Parsing
    // ========================================================================

    fn parseNumber(self: *Self) ComptimeError!ComptimeValue {
        const start = self.pos;

        // Handle hex, octal, binary
        if (self.peek() == '0' and self.pos + 1 < self.source.len) {
            const next = self.source[self.pos + 1];
            if (next == 'x' or next == 'X') {
                self.pos += 2;
                return self.parseHex();
            }
            if (next == 'o' or next == 'O') {
                self.pos += 2;
                return self.parseOctal();
            }
            if (next == 'b' or next == 'B') {
                self.pos += 2;
                return self.parseBinary();
            }
        }

        // Decimal number
        while (self.pos < self.source.len and (isDigit(self.source[self.pos]) or self.source[self.pos] == '_')) {
            self.advance();
        }

        // Decimal point
        if (self.pos < self.source.len and self.source[self.pos] == '.') {
            self.advance();
            while (self.pos < self.source.len and (isDigit(self.source[self.pos]) or self.source[self.pos] == '_')) {
                self.advance();
            }
        }

        // Exponent
        if (self.pos < self.source.len and (self.source[self.pos] == 'e' or self.source[self.pos] == 'E')) {
            self.advance();
            if (self.pos < self.source.len and (self.source[self.pos] == '+' or self.source[self.pos] == '-')) {
                self.advance();
            }
            while (self.pos < self.source.len and isDigit(self.source[self.pos])) {
                self.advance();
            }
        }

        // Remove underscores for parsing
        const num_str = self.source[start..self.pos];
        var clean: std.ArrayList(u8) = .empty;
        defer clean.deinit(self.allocator);
        for (num_str) |ch| {
            if (ch != '_') clean.append(self.allocator, ch) catch return ComptimeError.OutOfMemory;
        }

        const value = std.fmt.parseFloat(f64, clean.items) catch return ComptimeError.InvalidNumber;
        return .{ .number = value };
    }

    fn parseHex(self: *Self) ComptimeError!ComptimeValue {
        var value: u64 = 0;
        var has_digit = false;
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '_') {
                self.advance();
                continue;
            }
            const digit = switch (c) {
                '0'...'9' => c - '0',
                'a'...'f' => c - 'a' + 10,
                'A'...'F' => c - 'A' + 10,
                else => break,
            };
            value = value * 16 + digit;
            has_digit = true;
            self.advance();
        }
        if (!has_digit) return ComptimeError.InvalidNumber;
        return .{ .number = @floatFromInt(value) };
    }

    fn parseOctal(self: *Self) ComptimeError!ComptimeValue {
        var value: u64 = 0;
        var has_digit = false;
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '_') {
                self.advance();
                continue;
            }
            if (c < '0' or c > '7') break;
            value = value * 8 + (c - '0');
            has_digit = true;
            self.advance();
        }
        if (!has_digit) return ComptimeError.InvalidNumber;
        return .{ .number = @floatFromInt(value) };
    }

    fn parseBinary(self: *Self) ComptimeError!ComptimeValue {
        var value: u64 = 0;
        var has_digit = false;
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '_') {
                self.advance();
                continue;
            }
            if (c != '0' and c != '1') break;
            value = value * 2 + (c - '0');
            has_digit = true;
            self.advance();
        }
        if (!has_digit) return ComptimeError.InvalidNumber;
        return .{ .number = @floatFromInt(value) };
    }

    fn parseString(self: *Self, quote: u8) ComptimeError!ComptimeValue {
        self.advance(); // Opening quote
        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == quote) {
                self.advance();
                const str = buf.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory;
                return .{ .string = str };
            }
            if (c == '\\') {
                self.advance();
                if (self.pos >= self.source.len) return ComptimeError.InvalidEscape;
                const escaped = switch (self.source[self.pos]) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '\'' => '\'',
                    '"' => '"',
                    '0' => 0,
                    'x' => blk: {
                        self.advance();
                        if (self.pos + 1 >= self.source.len) return ComptimeError.InvalidEscape;
                        const hex = self.source[self.pos .. self.pos + 2];
                        self.pos += 1; // Will advance again below
                        break :blk std.fmt.parseInt(u8, hex, 16) catch return ComptimeError.InvalidEscape;
                    },
                    else => self.source[self.pos],
                };
                buf.append(self.allocator, escaped) catch return ComptimeError.OutOfMemory;
            } else {
                buf.append(self.allocator, c) catch return ComptimeError.OutOfMemory;
            }
            self.advance();
        }
        return ComptimeError.UnclosedString;
    }

    fn parseTemplateLiteral(self: *Self) ComptimeError!ComptimeValue {
        self.advance(); // Opening backtick
        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == '`') {
                self.advance();
                const str = buf.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory;
                return .{ .string = str };
            }
            if (c == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                // Template interpolation - not supported in comptime
                return ComptimeError.UnsupportedOp;
            }
            if (c == '\\') {
                self.advance();
                if (self.pos >= self.source.len) return ComptimeError.InvalidEscape;
                const escaped = switch (self.source[self.pos]) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '`' => '`',
                    '$' => '$',
                    else => self.source[self.pos],
                };
                buf.append(self.allocator, escaped) catch return ComptimeError.OutOfMemory;
            } else {
                buf.append(self.allocator, c) catch return ComptimeError.OutOfMemory;
            }
            self.advance();
        }
        return ComptimeError.UnclosedString;
    }

    fn parseArray(self: *Self) ComptimeError!ComptimeValue {
        self.expect('[') catch return ComptimeError.SyntaxError;
        self.skipWhitespace();

        var elements: std.ArrayList(ComptimeValue) = .empty;
        errdefer {
            for (elements.items) |elem| {
                elem.deinit(self.allocator);
            }
            elements.deinit(self.allocator);
        }

        while (self.pos < self.source.len and self.peek() != ']') {
            const elem = try self.parseExpression(.none);
            elements.append(self.allocator, elem) catch {
                elem.deinit(self.allocator);
                return ComptimeError.OutOfMemory;
            };
            self.skipWhitespace();

            if (self.peek() == ',') {
                self.advance();
                self.skipWhitespace();
            } else {
                break;
            }
        }

        self.expect(']') catch return ComptimeError.UnclosedBracket;
        const arr = elements.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory;
        return .{ .array = arr };
    }

    fn parseObject(self: *Self) ComptimeError!ComptimeValue {
        self.expect('{') catch return ComptimeError.SyntaxError;
        self.skipWhitespace();

        var props: std.ArrayList(ComptimeValue.ObjectProperty) = .empty;
        errdefer {
            for (props.items) |prop| {
                self.allocator.free(prop.key);
                prop.value.deinit(self.allocator);
            }
            props.deinit(self.allocator);
        }

        while (self.pos < self.source.len and self.peek() != '}') {
            // Parse key (identifier or string)
            self.skipWhitespace();
            const key = blk: {
                if (self.peek() == '"' or self.peek() == '\'') {
                    const str_val = try self.parseString(self.peek());
                    break :blk str_val.string;
                } else if (isIdentifierStart(self.peek())) {
                    // Always allocate key for consistent ownership
                    const ident = self.scanIdentifier();
                    break :blk self.allocator.dupe(u8, ident) catch return ComptimeError.OutOfMemory;
                } else {
                    return ComptimeError.SyntaxError;
                }
            };

            self.skipWhitespace();
            self.expect(':') catch {
                self.allocator.free(key);
                return ComptimeError.SyntaxError;
            };
            self.skipWhitespace();

            const value = self.parseExpression(.none) catch |err| {
                self.allocator.free(key);
                return err;
            };
            props.append(self.allocator, .{ .key = key, .value = value }) catch {
                self.allocator.free(key);
                value.deinit(self.allocator);
                return ComptimeError.OutOfMemory;
            };

            self.skipWhitespace();
            if (self.peek() == ',') {
                self.advance();
                self.skipWhitespace();
            } else {
                break;
            }
        }

        self.expect('}') catch return ComptimeError.UnclosedBrace;
        const obj = props.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory;
        return .{ .object = obj };
    }

    fn parseIdentifier(self: *Self) ComptimeError!ComptimeValue {
        const ident = self.scanIdentifier();

        // Keywords and special values
        if (std.mem.eql(u8, ident, "true")) return .{ .boolean = true };
        if (std.mem.eql(u8, ident, "false")) return .{ .boolean = false };
        if (std.mem.eql(u8, ident, "null")) return .{ .null_val = {} };
        if (std.mem.eql(u8, ident, "undefined")) return .{ .undefined_val = {} };
        if (std.mem.eql(u8, ident, "NaN")) return .{ .nan_val = {} };
        if (std.mem.eql(u8, ident, "Infinity")) return .{ .infinity = .{ .negative = false } };

        // Build metadata
        if (std.mem.eql(u8, ident, "__BUILD_TIME__")) {
            return if (self.build_time) |bt| .{ .string = bt } else .{ .undefined_val = {} };
        }
        if (std.mem.eql(u8, ident, "__GIT_COMMIT__")) {
            return if (self.git_commit) |gc| .{ .string = gc } else .{ .undefined_val = {} };
        }
        if (std.mem.eql(u8, ident, "__VERSION__")) {
            return if (self.version) |v| .{ .string = v } else .{ .undefined_val = {} };
        }

        // Check for Math, Env, JSON, etc.
        self.skipWhitespace();
        if (self.peek() == '.') {
            self.advance();
            self.skipWhitespace();

            if (std.mem.eql(u8, ident, "Math")) {
                return self.parseMathAccess();
            }
            if (std.mem.eql(u8, ident, "Env")) {
                return self.parseEnvAccess();
            }
            if (std.mem.eql(u8, ident, "JSON")) {
                return self.parseJSONAccess();
            }
        }

        // Check for function call on identifier (e.g., hash(...), parseInt(...))
        if (self.peek() == '(') {
            if (std.mem.eql(u8, ident, "hash")) {
                return self.parseHashCall();
            }
            if (std.mem.eql(u8, ident, "parseInt")) {
                return self.parseParseIntCall();
            }
            if (std.mem.eql(u8, ident, "parseFloat")) {
                return self.parseParseFloatCall();
            }
        }

        return ComptimeError.UnknownIdentifier;
    }

    // ========================================================================
    // Math Support
    // ========================================================================

    fn parseMathAccess(self: *Self) ComptimeError!ComptimeValue {
        const name = self.scanIdentifier();
        self.skipWhitespace();

        // Math constants
        if (std.mem.eql(u8, name, "PI")) return .{ .number = 3.141592653589793 };
        if (std.mem.eql(u8, name, "E")) return .{ .number = 2.718281828459045 };
        if (std.mem.eql(u8, name, "LN2")) return .{ .number = 0.6931471805599453 };
        if (std.mem.eql(u8, name, "LN10")) return .{ .number = 2.302585092994046 };
        if (std.mem.eql(u8, name, "LOG2E")) return .{ .number = 1.4426950408889634 };
        if (std.mem.eql(u8, name, "LOG10E")) return .{ .number = 0.4342944819032518 };
        if (std.mem.eql(u8, name, "SQRT2")) return .{ .number = 1.4142135623730951 };
        if (std.mem.eql(u8, name, "SQRT1_2")) return .{ .number = 0.7071067811865476 };

        // Math functions
        if (self.peek() != '(') return ComptimeError.UnknownIdentifier;

        // Disallowed functions
        if (std.mem.eql(u8, name, "random")) return ComptimeError.CallNotAllowed;

        const args = try self.parseCallArgs();
        defer self.freeArgs(args);
        return self.evaluateMathCall(name, args);
    }

    fn evaluateMathCall(self: *Self, name: []const u8, args: []const ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        // Single-argument functions
        if (args.len >= 1) {
            const n = args[0].toNumber() orelse return ComptimeError.TypeMismatch;

            if (std.mem.eql(u8, name, "abs")) return .{ .number = @abs(n) };
            if (std.mem.eql(u8, name, "floor")) return .{ .number = @floor(n) };
            if (std.mem.eql(u8, name, "ceil")) return .{ .number = @ceil(n) };
            if (std.mem.eql(u8, name, "round")) return .{ .number = @round(n) };
            if (std.mem.eql(u8, name, "trunc")) return .{ .number = @trunc(n) };
            if (std.mem.eql(u8, name, "sqrt")) {
                if (n < 0) return .{ .nan_val = {} };
                return .{ .number = @sqrt(n) };
            }
            if (std.mem.eql(u8, name, "cbrt")) return .{ .number = std.math.cbrt(n) };
            if (std.mem.eql(u8, name, "sin")) return .{ .number = @sin(n) };
            if (std.mem.eql(u8, name, "cos")) return .{ .number = @cos(n) };
            if (std.mem.eql(u8, name, "tan")) return .{ .number = @tan(n) };
            if (std.mem.eql(u8, name, "asin")) return .{ .number = std.math.asin(n) };
            if (std.mem.eql(u8, name, "acos")) return .{ .number = std.math.acos(n) };
            if (std.mem.eql(u8, name, "atan")) return .{ .number = std.math.atan(n) };
            if (std.mem.eql(u8, name, "sinh")) return .{ .number = std.math.sinh(n) };
            if (std.mem.eql(u8, name, "cosh")) return .{ .number = std.math.cosh(n) };
            if (std.mem.eql(u8, name, "tanh")) return .{ .number = std.math.tanh(n) };
            if (std.mem.eql(u8, name, "asinh")) return .{ .number = std.math.asinh(n) };
            if (std.mem.eql(u8, name, "acosh")) return .{ .number = std.math.acosh(n) };
            if (std.mem.eql(u8, name, "atanh")) return .{ .number = std.math.atanh(n) };
            if (std.mem.eql(u8, name, "log")) return .{ .number = @log(n) };
            if (std.mem.eql(u8, name, "log2")) return .{ .number = std.math.log2(n) };
            if (std.mem.eql(u8, name, "log10")) return .{ .number = std.math.log10(n) };
            if (std.mem.eql(u8, name, "log1p")) return .{ .number = std.math.log1p(n) };
            if (std.mem.eql(u8, name, "exp")) return .{ .number = @exp(n) };
            if (std.mem.eql(u8, name, "expm1")) return .{ .number = std.math.expm1(n) };
            if (std.mem.eql(u8, name, "sign")) {
                if (std.math.isNan(n)) return .{ .nan_val = {} };
                if (n > 0) return .{ .number = 1 };
                if (n < 0) return .{ .number = -1 };
                return .{ .number = 0 };
            }
            if (std.mem.eql(u8, name, "clz32")) {
                const i: u32 = @bitCast(@as(i32, @intFromFloat(@trunc(n))));
                return .{ .number = @floatFromInt(@clz(i)) };
            }
            if (std.mem.eql(u8, name, "fround")) {
                const f: f32 = @floatCast(n);
                return .{ .number = @floatCast(f) };
            }
        }

        // Two-argument functions
        if (args.len >= 2) {
            const a = args[0].toNumber() orelse return ComptimeError.TypeMismatch;
            const b = args[1].toNumber() orelse return ComptimeError.TypeMismatch;

            if (std.mem.eql(u8, name, "pow")) return .{ .number = std.math.pow(f64, a, b) };
            if (std.mem.eql(u8, name, "atan2")) return .{ .number = std.math.atan2(a, b) };
            if (std.mem.eql(u8, name, "hypot")) return .{ .number = std.math.hypot(a, b) };
            if (std.mem.eql(u8, name, "imul")) {
                const ai: i32 = @intFromFloat(@trunc(a));
                const bi: i32 = @intFromFloat(@trunc(b));
                return .{ .number = @floatFromInt(ai *% bi) };
            }
        }

        // Variadic: min, max
        if (std.mem.eql(u8, name, "min") or std.mem.eql(u8, name, "max")) {
            if (args.len == 0) return .{ .infinity = .{ .negative = std.mem.eql(u8, name, "max") } };

            var result = args[0].toNumber() orelse return ComptimeError.TypeMismatch;
            for (args[1..]) |arg| {
                const v = arg.toNumber() orelse return ComptimeError.TypeMismatch;
                if (std.math.isNan(v)) return .{ .nan_val = {} };
                result = if (std.mem.eql(u8, name, "min")) @min(result, v) else @max(result, v);
            }
            return .{ .number = result };
        }

        return ComptimeError.CallNotAllowed;
    }

    // ========================================================================
    // Environment Variable Support
    // ========================================================================

    fn parseEnvAccess(self: *Self) ComptimeError!ComptimeValue {
        const name = self.scanIdentifier();
        if (self.env) |env_map| {
            if (env_map.get(name)) |value| {
                return .{ .string = value };
            }
        }
        return .{ .undefined_val = {} };
    }

    // ========================================================================
    // JSON Support
    // ========================================================================

    fn parseJSONAccess(self: *Self) ComptimeError!ComptimeValue {
        const name = self.scanIdentifier();
        self.skipWhitespace();

        if (std.mem.eql(u8, name, "parse") and self.peek() == '(') {
            return self.parseJSONParse();
        }

        return ComptimeError.CallNotAllowed;
    }

    fn parseJSONParse(self: *Self) ComptimeError!ComptimeValue {
        const args = try self.parseCallArgs();
        defer self.freeArgs(args);

        if (args.len < 1) return ComptimeError.SyntaxError;

        const json_str = switch (args[0]) {
            .string => |s| s,
            else => return ComptimeError.TypeMismatch,
        };

        // Parse JSON string as comptime value
        var json_parser = Self.init(self.allocator, json_str, self.line, self.col);
        return json_parser.parseJSONValue();
    }

    fn parseJSONValue(self: *Self) ComptimeError!ComptimeValue {
        self.skipWhitespace();
        if (self.pos >= self.source.len) return ComptimeError.UnexpectedEnd;

        const c = self.source[self.pos];
        if (c == '"') return self.parseString('"');
        if (c == '[') return self.parseArray();
        if (c == '{') return self.parseObject();
        if (c == 't' or c == 'f') return self.parseIdentifier();
        if (c == 'n') return self.parseIdentifier();
        if (c == '-' or isDigit(c)) return self.parseNumber();

        return ComptimeError.SyntaxError;
    }

    // ========================================================================
    // Hash Function (FNV-1a)
    // ========================================================================

    fn parseHashCall(self: *Self) ComptimeError!ComptimeValue {
        const args = try self.parseCallArgs();
        defer self.freeArgs(args);

        if (args.len < 1) return ComptimeError.SyntaxError;

        const input = switch (args[0]) {
            .string => |s| s,
            else => return ComptimeError.TypeMismatch,
        };

        // FNV-1a 32-bit hash
        const hash = fnv1a(input);

        // Convert to 8-char hex string
        var buf: [8]u8 = undefined;
        _ = std.fmt.bufPrint(&buf, "{x:0>8}", .{hash}) catch return ComptimeError.OutOfMemory;

        const result = self.allocator.dupe(u8, &buf) catch return ComptimeError.OutOfMemory;
        return .{ .string = result };
    }

    fn fnv1a(data: []const u8) u32 {
        var hash: u32 = 2166136261; // FNV offset basis
        for (data) |byte| {
            hash ^= byte;
            hash *%= 16777619; // FNV prime
        }
        return hash;
    }

    // ========================================================================
    // parseInt / parseFloat
    // ========================================================================

    fn parseParseIntCall(self: *Self) ComptimeError!ComptimeValue {
        const args = try self.parseCallArgs();
        defer self.freeArgs(args);

        if (args.len < 1) return .{ .nan_val = {} };

        switch (args[0]) {
            .string => |s| {
                const radix: u8 = if (args.len >= 2) blk: {
                    const r = args[1].toNumber() orelse break :blk 10;
                    break :blk @intFromFloat(@trunc(r));
                } else 10;

                const trimmed = std.mem.trim(u8, s, " \t\n\r");
                const value = std.fmt.parseInt(i64, trimmed, radix) catch return .{ .nan_val = {} };
                return .{ .number = @floatFromInt(value) };
            },
            .number => |n| return .{ .number = @trunc(n) },
            else => return .{ .nan_val = {} },
        }
    }

    fn parseParseFloatCall(self: *Self) ComptimeError!ComptimeValue {
        const args = try self.parseCallArgs();
        defer self.freeArgs(args);

        if (args.len < 1) return .{ .nan_val = {} };

        switch (args[0]) {
            .string => |s| {
                const trimmed = std.mem.trim(u8, s, " \t\n\r");
                const value = std.fmt.parseFloat(f64, trimmed) catch return .{ .nan_val = {} };
                return .{ .number = value };
            },
            .number => |n| return .{ .number = n },
            else => return .{ .nan_val = {} },
        }
    }

    // ========================================================================
    // Call Argument Parsing
    // ========================================================================

    fn parseCallArgs(self: *Self) ComptimeError![]const ComptimeValue {
        self.expect('(') catch return ComptimeError.SyntaxError;
        self.skipWhitespace();

        var args: std.ArrayList(ComptimeValue) = .empty;
        errdefer {
            for (args.items) |arg| {
                arg.deinit(self.allocator);
            }
            args.deinit(self.allocator);
        }

        while (self.pos < self.source.len and self.peek() != ')') {
            const arg = try self.parseExpression(.none);
            args.append(self.allocator, arg) catch {
                arg.deinit(self.allocator);
                return ComptimeError.OutOfMemory;
            };
            self.skipWhitespace();

            if (self.peek() == ',') {
                self.advance();
                self.skipWhitespace();
            } else {
                break;
            }
        }

        self.expect(')') catch return ComptimeError.UnclosedParen;
        return args.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory;
    }

    /// Free argument list returned by parseCallArgs
    fn freeArgs(self: *Self, args: []const ComptimeValue) void {
        for (args) |arg| {
            arg.deinit(self.allocator);
        }
        self.allocator.free(args);
    }

    // ========================================================================
    // Arithmetic Operations
    // ========================================================================

    fn add(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        // String concatenation
        if (left == .string or right == .string) {
            const ls = valueToString(left);
            const rs = valueToString(right);
            const result = std.fmt.allocPrint(std.heap.page_allocator, "{s}{s}", .{ ls, rs }) catch return ComptimeError.OutOfMemory;
            return .{ .string = result };
        }

        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        return .{ .number = ln + rn };
    }

    fn subtract(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        return .{ .number = ln - rn };
    }

    fn multiply(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        return .{ .number = ln * rn };
    }

    fn divide(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        return .{ .number = ln / rn };
    }

    fn modulo(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        return .{ .number = @mod(ln, rn) };
    }

    // ========================================================================
    // Bitwise Operations
    // ========================================================================

    fn bitwiseOr(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        const li: i32 = @intFromFloat(@trunc(ln));
        const ri: i32 = @intFromFloat(@trunc(rn));
        return .{ .number = @floatFromInt(li | ri) };
    }

    fn bitwiseAnd(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        const li: i32 = @intFromFloat(@trunc(ln));
        const ri: i32 = @intFromFloat(@trunc(rn));
        return .{ .number = @floatFromInt(li & ri) };
    }

    fn bitwiseXor(self: *Self, left: ComptimeValue, right: ComptimeValue) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        const li: i32 = @intFromFloat(@trunc(ln));
        const ri: i32 = @intFromFloat(@trunc(rn));
        return .{ .number = @floatFromInt(li ^ ri) };
    }

    const ShiftDir = enum { left, right, unsigned_right };

    fn bitwiseShift(self: *Self, left: ComptimeValue, right: ComptimeValue, dir: ShiftDir) ComptimeError!ComptimeValue {
        _ = self;
        const ln = left.toNumber() orelse return ComptimeError.TypeMismatch;
        const rn = right.toNumber() orelse return ComptimeError.TypeMismatch;
        const li: i32 = @intFromFloat(@trunc(ln));
        const shift: u5 = @intCast(@as(u32, @bitCast(@as(i32, @intFromFloat(@trunc(rn))))) & 0x1f);

        return switch (dir) {
            .left => .{ .number = @floatFromInt(li << shift) },
            .right => .{ .number = @floatFromInt(li >> shift) },
            .unsigned_right => blk: {
                const ui: u32 = @bitCast(li);
                break :blk .{ .number = @floatFromInt(ui >> shift) };
            },
        };
    }

    // ========================================================================
    // Member Access (for method chaining on strings)
    // ========================================================================

    fn handleMemberAccess(self: *Self, left: ComptimeValue) ComptimeError!ComptimeValue {
        // Parse property/method name
        const name = self.scanIdentifier();
        if (name.len == 0) return ComptimeError.SyntaxError;

        self.skipWhitespace();

        // String member access
        if (left == .string) {
            const str = left.string;

            // Properties
            if (std.mem.eql(u8, name, "length")) {
                const len = str.len; // Save before freeing
                // Free the original string since we're consuming it
                left.deinit(self.allocator);
                return .{ .number = @floatFromInt(len) };
            }

            // Methods (require parentheses)
            if (self.peek() == '(') {
                const result = try self.handleStringMethod(str, name);
                // Free the original string since we've consumed it
                left.deinit(self.allocator);
                return result;
            }

            return ComptimeError.UnknownIdentifier;
        }

        // Array member access
        if (left == .array) {
            const arr = left.array;

            // Properties
            if (std.mem.eql(u8, name, "length")) {
                const len = arr.len; // Save before freeing
                // Free the original array since we're consuming it
                left.deinit(self.allocator);
                return .{ .number = @floatFromInt(len) };
            }

            return ComptimeError.UnknownIdentifier;
        }

        return ComptimeError.UnsupportedOp;
    }

    fn handleStringMethod(self: *Self, str: []const u8, name: []const u8) ComptimeError!ComptimeValue {
        const args = try self.parseCallArgs();
        defer self.freeArgs(args);

        // No-argument methods
        if (std.mem.eql(u8, name, "toUpperCase")) {
            return self.stringToUpperCase(str);
        }
        if (std.mem.eql(u8, name, "toLowerCase")) {
            return self.stringToLowerCase(str);
        }
        if (std.mem.eql(u8, name, "trim")) {
            return self.stringTrim(str);
        }
        if (std.mem.eql(u8, name, "trimStart") or std.mem.eql(u8, name, "trimLeft")) {
            return self.stringTrimStart(str);
        }
        if (std.mem.eql(u8, name, "trimEnd") or std.mem.eql(u8, name, "trimRight")) {
            return self.stringTrimEnd(str);
        }

        // Single-argument methods
        if (args.len >= 1) {
            if (std.mem.eql(u8, name, "includes")) {
                const search = switch (args[0]) {
                    .string => |s| s,
                    else => return ComptimeError.TypeMismatch,
                };
                return .{ .boolean = std.mem.indexOf(u8, str, search) != null };
            }
            if (std.mem.eql(u8, name, "startsWith")) {
                const search = switch (args[0]) {
                    .string => |s| s,
                    else => return ComptimeError.TypeMismatch,
                };
                return .{ .boolean = std.mem.startsWith(u8, str, search) };
            }
            if (std.mem.eql(u8, name, "endsWith")) {
                const search = switch (args[0]) {
                    .string => |s| s,
                    else => return ComptimeError.TypeMismatch,
                };
                return .{ .boolean = std.mem.endsWith(u8, str, search) };
            }
            if (std.mem.eql(u8, name, "indexOf")) {
                const search = switch (args[0]) {
                    .string => |s| s,
                    else => return ComptimeError.TypeMismatch,
                };
                if (std.mem.indexOf(u8, str, search)) |idx| {
                    return .{ .number = @floatFromInt(idx) };
                }
                return .{ .number = -1 };
            }
            if (std.mem.eql(u8, name, "repeat")) {
                const count = args[0].toNumber() orelse return ComptimeError.TypeMismatch;
                if (count < 0 or count > 10000) return ComptimeError.TypeMismatch;
                const n: usize = @intFromFloat(@trunc(count));
                var result: std.ArrayList(u8) = .empty;
                for (0..n) |_| {
                    result.appendSlice(self.allocator, str) catch return ComptimeError.OutOfMemory;
                }
                return .{ .string = result.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory };
            }
            if (std.mem.eql(u8, name, "split")) {
                const delim = switch (args[0]) {
                    .string => |s| s,
                    else => return ComptimeError.TypeMismatch,
                };
                return self.stringSplit(str, delim);
            }
            if (std.mem.eql(u8, name, "charAt")) {
                const idx_f = args[0].toNumber() orelse return ComptimeError.TypeMismatch;
                const idx: usize = @intFromFloat(@trunc(idx_f));
                if (idx >= str.len) {
                    const empty = self.allocator.alloc(u8, 0) catch return ComptimeError.OutOfMemory;
                    return .{ .string = empty };
                }
                const result = self.allocator.alloc(u8, 1) catch return ComptimeError.OutOfMemory;
                result[0] = str[idx];
                return .{ .string = result };
            }
        }

        // slice(start, end?)
        if (std.mem.eql(u8, name, "slice")) {
            return self.stringSlice(str, args);
        }

        // substring(start, end?)
        if (std.mem.eql(u8, name, "substring")) {
            return self.stringSlice(str, args);
        }

        // padStart(length, padStr?)
        if (std.mem.eql(u8, name, "padStart")) {
            return self.stringPadStart(str, args);
        }

        // padEnd(length, padStr?)
        if (std.mem.eql(u8, name, "padEnd")) {
            return self.stringPadEnd(str, args);
        }

        // replace(search, replacement)
        if (std.mem.eql(u8, name, "replace")) {
            if (args.len < 2) return ComptimeError.SyntaxError;
            const search = switch (args[0]) {
                .string => |s| s,
                else => return ComptimeError.TypeMismatch,
            };
            const replacement = switch (args[1]) {
                .string => |s| s,
                else => return ComptimeError.TypeMismatch,
            };
            return self.stringReplace(str, search, replacement, false);
        }

        // replaceAll(search, replacement)
        if (std.mem.eql(u8, name, "replaceAll")) {
            if (args.len < 2) return ComptimeError.SyntaxError;
            const search = switch (args[0]) {
                .string => |s| s,
                else => return ComptimeError.TypeMismatch,
            };
            const replacement = switch (args[1]) {
                .string => |s| s,
                else => return ComptimeError.TypeMismatch,
            };
            return self.stringReplace(str, search, replacement, true);
        }

        return ComptimeError.CallNotAllowed;
    }

    // String helper methods
    fn stringToUpperCase(self: *Self, str: []const u8) ComptimeError!ComptimeValue {
        const result = self.allocator.alloc(u8, str.len) catch return ComptimeError.OutOfMemory;
        for (str, 0..) |c, i| {
            result[i] = std.ascii.toUpper(c);
        }
        return .{ .string = result };
    }

    fn stringToLowerCase(self: *Self, str: []const u8) ComptimeError!ComptimeValue {
        const result = self.allocator.alloc(u8, str.len) catch return ComptimeError.OutOfMemory;
        for (str, 0..) |c, i| {
            result[i] = std.ascii.toLower(c);
        }
        return .{ .string = result };
    }

    fn stringTrim(self: *Self, str: []const u8) ComptimeError!ComptimeValue {
        const trimmed = std.mem.trim(u8, str, " \t\n\r");
        const result = self.allocator.dupe(u8, trimmed) catch return ComptimeError.OutOfMemory;
        return .{ .string = result };
    }

    fn stringTrimStart(self: *Self, str: []const u8) ComptimeError!ComptimeValue {
        // Trim leading whitespace
        var start: usize = 0;
        for (str) |c| {
            if (c != ' ' and c != '\t' and c != '\n' and c != '\r') break;
            start += 1;
        }
        const result = self.allocator.dupe(u8, str[start..]) catch return ComptimeError.OutOfMemory;
        return .{ .string = result };
    }

    fn stringTrimEnd(self: *Self, str: []const u8) ComptimeError!ComptimeValue {
        // Trim trailing whitespace
        var end: usize = str.len;
        while (end > 0) {
            const c = str[end - 1];
            if (c != ' ' and c != '\t' and c != '\n' and c != '\r') break;
            end -= 1;
        }
        const result = self.allocator.dupe(u8, str[0..end]) catch return ComptimeError.OutOfMemory;
        return .{ .string = result };
    }

    fn stringSlice(self: *Self, str: []const u8, args: []const ComptimeValue) ComptimeError!ComptimeValue {
        if (args.len < 1) return ComptimeError.SyntaxError;

        const start_f = args[0].toNumber() orelse return ComptimeError.TypeMismatch;
        var start: i64 = @intFromFloat(@trunc(start_f));
        const len: i64 = @intCast(str.len);

        // Handle negative start
        if (start < 0) {
            start = @max(0, len + start);
        }
        if (start > len) start = len;

        var end: i64 = len;
        if (args.len >= 2) {
            const end_f = args[1].toNumber() orelse return ComptimeError.TypeMismatch;
            end = @intFromFloat(@trunc(end_f));
            if (end < 0) {
                end = @max(0, len + end);
            }
            if (end > len) end = len;
        }

        if (end < start) {
            const empty = self.allocator.alloc(u8, 0) catch return ComptimeError.OutOfMemory;
            return .{ .string = empty };
        }

        const start_u: usize = @intCast(start);
        const end_u: usize = @intCast(end);
        const result = self.allocator.dupe(u8, str[start_u..end_u]) catch return ComptimeError.OutOfMemory;
        return .{ .string = result };
    }

    fn stringSplit(self: *Self, str: []const u8, delim: []const u8) ComptimeError!ComptimeValue {
        var parts: std.ArrayList(ComptimeValue) = .empty;
        errdefer {
            for (parts.items) |p| p.deinit(self.allocator);
            parts.deinit(self.allocator);
        }

        if (delim.len == 0) {
            // Split into individual characters
            for (str) |c| {
                const char_str = self.allocator.alloc(u8, 1) catch return ComptimeError.OutOfMemory;
                char_str[0] = c;
                parts.append(self.allocator, .{ .string = char_str }) catch return ComptimeError.OutOfMemory;
            }
        } else {
            var iter = std.mem.splitSequence(u8, str, delim);
            while (iter.next()) |part| {
                const part_copy = self.allocator.dupe(u8, part) catch return ComptimeError.OutOfMemory;
                parts.append(self.allocator, .{ .string = part_copy }) catch return ComptimeError.OutOfMemory;
            }
        }

        return .{ .array = parts.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory };
    }

    fn stringPadStart(self: *Self, str: []const u8, args: []const ComptimeValue) ComptimeError!ComptimeValue {
        if (args.len < 1) return ComptimeError.SyntaxError;

        const target_len_f = args[0].toNumber() orelse return ComptimeError.TypeMismatch;
        const target_len: usize = @intFromFloat(@max(0, @trunc(target_len_f)));

        if (target_len <= str.len) {
            return .{ .string = self.allocator.dupe(u8, str) catch return ComptimeError.OutOfMemory };
        }

        const pad_str: []const u8 = if (args.len >= 2)
            switch (args[1]) {
                .string => |s| s,
                else => return ComptimeError.TypeMismatch,
            }
        else
            " ";

        if (pad_str.len == 0) {
            return .{ .string = self.allocator.dupe(u8, str) catch return ComptimeError.OutOfMemory };
        }

        const pad_len = target_len - str.len;
        var result: std.ArrayList(u8) = .empty;

        var i: usize = 0;
        while (i < pad_len) : (i += 1) {
            result.append(self.allocator, pad_str[i % pad_str.len]) catch return ComptimeError.OutOfMemory;
        }
        result.appendSlice(self.allocator, str) catch return ComptimeError.OutOfMemory;

        return .{ .string = result.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory };
    }

    fn stringPadEnd(self: *Self, str: []const u8, args: []const ComptimeValue) ComptimeError!ComptimeValue {
        if (args.len < 1) return ComptimeError.SyntaxError;

        const target_len_f = args[0].toNumber() orelse return ComptimeError.TypeMismatch;
        const target_len: usize = @intFromFloat(@max(0, @trunc(target_len_f)));

        if (target_len <= str.len) {
            return .{ .string = self.allocator.dupe(u8, str) catch return ComptimeError.OutOfMemory };
        }

        const pad_str: []const u8 = if (args.len >= 2)
            switch (args[1]) {
                .string => |s| s,
                else => return ComptimeError.TypeMismatch,
            }
        else
            " ";

        if (pad_str.len == 0) {
            return .{ .string = self.allocator.dupe(u8, str) catch return ComptimeError.OutOfMemory };
        }

        const pad_len = target_len - str.len;
        var result: std.ArrayList(u8) = .empty;
        result.appendSlice(self.allocator, str) catch return ComptimeError.OutOfMemory;

        var i: usize = 0;
        while (i < pad_len) : (i += 1) {
            result.append(self.allocator, pad_str[i % pad_str.len]) catch return ComptimeError.OutOfMemory;
        }

        return .{ .string = result.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory };
    }

    fn stringReplace(self: *Self, str: []const u8, search: []const u8, replacement: []const u8, replace_all: bool) ComptimeError!ComptimeValue {
        if (search.len == 0) {
            return .{ .string = self.allocator.dupe(u8, str) catch return ComptimeError.OutOfMemory };
        }

        var result: std.ArrayList(u8) = .empty;
        var i: usize = 0;
        var replaced = false;

        while (i < str.len) {
            if (i + search.len <= str.len and std.mem.eql(u8, str[i .. i + search.len], search)) {
                if (!replaced or replace_all) {
                    result.appendSlice(self.allocator, replacement) catch return ComptimeError.OutOfMemory;
                    replaced = true;
                    i += search.len;
                    continue;
                }
            }
            result.append(self.allocator, str[i]) catch return ComptimeError.OutOfMemory;
            i += 1;
        }

        return .{ .string = result.toOwnedSlice(self.allocator) catch return ComptimeError.OutOfMemory };
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn peek(self: *Self) u8 {
        if (self.pos >= self.source.len) return 0;
        return self.source[self.pos];
    }

    fn advance(self: *Self) void {
        if (self.pos < self.source.len) {
            if (self.source[self.pos] == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn expect(self: *Self, char: u8) ComptimeError!void {
        if (self.peek() != char) return ComptimeError.UnexpectedToken;
        self.advance();
    }

    fn skipWhitespace(self: *Self) void {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scanIdentifier(self: *Self) []const u8 {
        const start = self.pos;
        while (self.pos < self.source.len and isIdentifierChar(self.source[self.pos])) {
            self.advance();
        }
        return self.source[start..self.pos];
    }
};

// ============================================================================
// Literal Emission
// ============================================================================

/// Convert a ComptimeValue back to JavaScript source code
pub fn emitLiteral(allocator: std.mem.Allocator, value: ComptimeValue) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(allocator);

    try emitValue(&buf, value, allocator);
    return buf.toOwnedSlice(allocator);
}

fn emitValue(buf: *std.ArrayList(u8), value: ComptimeValue, allocator: std.mem.Allocator) !void {
    switch (value) {
        .number => |n| {
            if (std.math.isNan(n)) {
                try buf.appendSlice(allocator, "NaN");
            } else if (std.math.isInf(n)) {
                if (n < 0) {
                    try buf.appendSlice(allocator, "-Infinity");
                } else {
                    try buf.appendSlice(allocator, "Infinity");
                }
            } else if (@trunc(n) == n and @abs(n) < 2147483647) {
                // Integer
                var num_buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&num_buf, "{d}", .{@as(i32, @intFromFloat(n))}) catch return error.OutOfMemory;
                try buf.appendSlice(allocator, s);
            } else {
                // Float
                var num_buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&num_buf, "{d}", .{n}) catch return error.OutOfMemory;
                try buf.appendSlice(allocator, s);
            }
        },
        .string => |s| {
            try buf.append(allocator, '"');
            for (s) |c| {
                switch (c) {
                    '"' => try buf.appendSlice(allocator, "\\\""),
                    '\\' => try buf.appendSlice(allocator, "\\\\"),
                    '\n' => try buf.appendSlice(allocator, "\\n"),
                    '\r' => try buf.appendSlice(allocator, "\\r"),
                    '\t' => try buf.appendSlice(allocator, "\\t"),
                    else => try buf.append(allocator, c),
                }
            }
            try buf.append(allocator, '"');
        },
        .boolean => |b| {
            try buf.appendSlice(allocator, if (b) "true" else "false");
        },
        .null_val => try buf.appendSlice(allocator, "null"),
        .undefined_val => try buf.appendSlice(allocator, "undefined"),
        .nan_val => try buf.appendSlice(allocator, "NaN"),
        .infinity => |i| {
            if (i.negative) {
                try buf.appendSlice(allocator, "-Infinity");
            } else {
                try buf.appendSlice(allocator, "Infinity");
            }
        },
        .array => |arr| {
            try buf.append(allocator, '[');
            for (arr, 0..) |elem, i| {
                if (i > 0) try buf.append(allocator, ',');
                try emitValue(buf, elem, allocator);
            }
            try buf.append(allocator, ']');
        },
        .object => |props| {
            // Wrap in ({ }) for expression context safety
            try buf.appendSlice(allocator, "({");
            for (props, 0..) |prop, i| {
                if (i > 0) try buf.append(allocator, ',');
                // Emit key (quote if needed)
                if (needsQuotes(prop.key)) {
                    try buf.append(allocator, '"');
                    try buf.appendSlice(allocator, prop.key);
                    try buf.append(allocator, '"');
                } else {
                    try buf.appendSlice(allocator, prop.key);
                }
                try buf.append(allocator, ':');
                try emitValue(buf, prop.value, allocator);
            }
            try buf.appendSlice(allocator, "})");
        },
    }
}

fn needsQuotes(key: []const u8) bool {
    if (key.len == 0) return true;
    if (!isIdentifierStart(key[0])) return true;
    for (key[1..]) |c| {
        if (!isIdentifierChar(c)) return true;
    }
    return false;
}

// ============================================================================
// Character Classification
// ============================================================================

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isIdentifierStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
}

fn isIdentifierChar(c: u8) bool {
    return isIdentifierStart(c) or isDigit(c);
}

fn valueToString(value: ComptimeValue) []const u8 {
    return switch (value) {
        .string => |s| s,
        .number => |n| blk: {
            if (std.math.isNan(n)) break :blk "NaN";
            if (std.math.isInf(n)) break :blk if (n < 0) "-Infinity" else "Infinity";
            break :blk ""; // Would need allocation for proper conversion
        },
        .boolean => |b| if (b) "true" else "false",
        .null_val => "null",
        .undefined_val => "undefined",
        .nan_val => "NaN",
        .infinity => |i| if (i.negative) "-Infinity" else "Infinity",
        .array => "",
        .object => "[object Object]",
    };
}

// ============================================================================
// Tests
// ============================================================================

test "comptime number literals" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "42", 1, 1);
    const r1 = try eval1.evaluate();
    try std.testing.expectEqual(@as(f64, 42), r1.number);

    var eval2 = ComptimeEvaluator.init(allocator, "3.14", 1, 1);
    const r2 = try eval2.evaluate();
    try std.testing.expectApproxEqAbs(@as(f64, 3.14), r2.number, 0.001);

    var eval3 = ComptimeEvaluator.init(allocator, "0xFF", 1, 1);
    const r3 = try eval3.evaluate();
    try std.testing.expectEqual(@as(f64, 255), r3.number);
}

test "comptime arithmetic" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "1 + 2", 1, 1);
    const r1 = try eval1.evaluate();
    try std.testing.expectEqual(@as(f64, 3), r1.number);

    var eval2 = ComptimeEvaluator.init(allocator, "1 + 2 * 3", 1, 1);
    const r2 = try eval2.evaluate();
    try std.testing.expectEqual(@as(f64, 7), r2.number);

    var eval3 = ComptimeEvaluator.init(allocator, "2 ** 10", 1, 1);
    const r3 = try eval3.evaluate();
    try std.testing.expectEqual(@as(f64, 1024), r3.number);
}

test "comptime boolean operations" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "true && false", 1, 1);
    const r1 = try eval1.evaluate();
    try std.testing.expectEqual(false, r1.boolean);

    var eval2 = ComptimeEvaluator.init(allocator, "true || false", 1, 1);
    const r2 = try eval2.evaluate();
    try std.testing.expectEqual(true, r2.boolean);

    var eval3 = ComptimeEvaluator.init(allocator, "!true", 1, 1);
    const r3 = try eval3.evaluate();
    try std.testing.expectEqual(false, r3.boolean);
}

test "comptime comparison" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "5 > 3", 1, 1);
    const r1 = try eval1.evaluate();
    try std.testing.expectEqual(true, r1.boolean);

    var eval2 = ComptimeEvaluator.init(allocator, "5 === 5", 1, 1);
    const r2 = try eval2.evaluate();
    try std.testing.expectEqual(true, r2.boolean);
}

test "comptime ternary" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "true ? 1 : 2", 1, 1);
    const r1 = try eval1.evaluate();
    try std.testing.expectEqual(@as(f64, 1), r1.number);

    var eval2 = ComptimeEvaluator.init(allocator, "false ? 1 : 2", 1, 1);
    const r2 = try eval2.evaluate();
    try std.testing.expectEqual(@as(f64, 2), r2.number);
}

test "comptime Math" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "Math.PI", 1, 1);
    const r1 = try eval1.evaluate();
    try std.testing.expectApproxEqAbs(@as(f64, 3.141592653589793), r1.number, 0.0001);

    var eval2 = ComptimeEvaluator.init(allocator, "Math.abs(-5)", 1, 1);
    const r2 = try eval2.evaluate();
    try std.testing.expectEqual(@as(f64, 5), r2.number);

    var eval3 = ComptimeEvaluator.init(allocator, "Math.max(1, 5, 3)", 1, 1);
    const r3 = try eval3.evaluate();
    try std.testing.expectEqual(@as(f64, 5), r3.number);
}

test "comptime string" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "\"hello\"", 1, 1);
    const r1 = try eval1.evaluate();
    defer r1.deinit(allocator);
    try std.testing.expectEqualStrings("hello", r1.string);
}

test "comptime array" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "[1, 2, 3]", 1, 1);
    const r1 = try eval1.evaluate();
    defer r1.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 3), r1.array.len);
    try std.testing.expectEqual(@as(f64, 1), r1.array[0].number);
}

test "comptime hash" {
    const allocator = std.testing.allocator;

    var eval1 = ComptimeEvaluator.init(allocator, "hash(\"test\")", 1, 1);
    const r1 = try eval1.evaluate();
    defer r1.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 8), r1.string.len);
}

test "emit literal" {
    const allocator = std.testing.allocator;

    const s1 = try emitLiteral(allocator, .{ .number = 42 });
    defer allocator.free(s1);
    try std.testing.expectEqualStrings("42", s1);

    const s2 = try emitLiteral(allocator, .{ .boolean = true });
    defer allocator.free(s2);
    try std.testing.expectEqualStrings("true", s2);
}

test "comptime string methods" {
    const allocator = std.testing.allocator;

    // toUpperCase
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".toUpperCase()", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("HELLO", r.string);
    }

    // toLowerCase
    {
        var eval = ComptimeEvaluator.init(allocator, "\"HELLO\".toLowerCase()", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("hello", r.string);
    }

    // trim
    {
        var eval = ComptimeEvaluator.init(allocator, "\"  hello  \".trim()", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("hello", r.string);
    }

    // slice
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".slice(1, 4)", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("ell", r.string);
    }

    // includes
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".includes(\"ell\")", 1, 1);
        const r = try eval.evaluate();
        try std.testing.expect(r.boolean);
    }

    // startsWith
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".startsWith(\"he\")", 1, 1);
        const r = try eval.evaluate();
        try std.testing.expect(r.boolean);
    }

    // endsWith
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".endsWith(\"lo\")", 1, 1);
        const r = try eval.evaluate();
        try std.testing.expect(r.boolean);
    }

    // split
    {
        var eval = ComptimeEvaluator.init(allocator, "\"a,b,c\".split(\",\")", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqual(@as(usize, 3), r.array.len);
        try std.testing.expectEqualStrings("a", r.array[0].string);
        try std.testing.expectEqualStrings("b", r.array[1].string);
        try std.testing.expectEqualStrings("c", r.array[2].string);
    }

    // repeat
    {
        var eval = ComptimeEvaluator.init(allocator, "\"ab\".repeat(3)", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("ababab", r.string);
    }

    // replace
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".replace(\"l\", \"L\")", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("heLlo", r.string);
    }

    // replaceAll
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".replaceAll(\"l\", \"L\")", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("heLLo", r.string);
    }

    // length property
    {
        var eval = ComptimeEvaluator.init(allocator, "\"hello\".length", 1, 1);
        const r = try eval.evaluate();
        try std.testing.expectEqual(@as(f64, 5), r.number);
    }

    // padStart
    {
        var eval = ComptimeEvaluator.init(allocator, "\"5\".padStart(3, \"0\")", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("005", r.string);
    }

    // padEnd
    {
        var eval = ComptimeEvaluator.init(allocator, "\"5\".padEnd(3, \"0\")", 1, 1);
        const r = try eval.evaluate();
        defer r.deinit(allocator);
        try std.testing.expectEqualStrings("500", r.string);
    }
}

test "comptime string method chaining" {
    const allocator = std.testing.allocator;

    // Chain multiple string methods
    var eval = ComptimeEvaluator.init(allocator, "\"  hello world  \".trim().toUpperCase()", 1, 1);
    const r = try eval.evaluate();
    defer r.deinit(allocator);
    try std.testing.expectEqualStrings("HELLO WORLD", r.string);
}
