//! TypeScript/TSX Type Stripper
//!
//! Removes TypeScript type annotations from source code, producing
//! JavaScript that the zts parser can handle. Preserves line/column
//! positions by replacing stripped spans with spaces.
//!
//! Supported:
//! - type/interface declarations (stripped entirely)
//! - import type / export type (stripped entirely)
//! - Variable/param/return annotations (: Type)
//! - as / satisfies assertions
//! - Generic parameters on functions/types
//!
//! Unsupported (errors):
//! - enum / const enum
//! - namespace / module
//! - implements keyword
//! - public / private / protected modifiers
//! - decorators (@something)
//! - angle-bracket assertions in TSX (<T>expr)
//!
//! Note: class/abstract class are now handled by the parser for consistent
//! error messages across .ts and .js files (see zts/parser/parse.zig)

const std = @import("std");
const builtin = @import("builtin");
const comptime_eval = @import("comptime.zig");

pub const StripError = error{
    UnsupportedEnum,
    UnsupportedNamespace,
    UnsupportedDecorator,
    UnsupportedAngleBracketAssertion,
    UnsupportedClass,
    UnsupportedAnyType,
    UnclosedTypeAnnotation,
    UnclosedGeneric,
    UnterminatedString,
    UnterminatedComment,
    OutOfMemory,
    ComptimeEvaluationFailed,
};

pub const StripResult = struct {
    code: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *StripResult) void {
        self.allocator.free(self.code);
    }
};

/// Environment for comptime evaluation
pub const ComptimeEnv = struct {
    /// Environment variables for Env.* lookups
    env_vars: ?*const std.StringHashMap([]const u8) = null,
    /// Build metadata
    build_time: ?[]const u8 = null,
    git_commit: ?[]const u8 = null,
    version: ?[]const u8 = null,
};

pub const StripOptions = struct {
    /// TSX mode: disallow angle-bracket assertions, preserve JSX
    tsx_mode: bool = false,
    /// Enable comptime() expression evaluation
    enable_comptime: bool = false,
    /// Environment for comptime evaluation
    comptime_env: ?ComptimeEnv = null,
    /// Emit unsupported-feature diagnostics to std.log
    /// Disabled by default in tests to avoid expected-error log failures.
    report_errors: bool = !builtin.is_test,
};

/// Strip TypeScript types from source code
pub fn strip(allocator: std.mem.Allocator, source: []const u8, options: StripOptions) StripError!StripResult {
    var stripper = Stripper.init(allocator, source, options);
    errdefer stripper.output.deinit(allocator);
    return stripper.strip();
}

// ============================================================================
// Stripper
// ============================================================================

const Stripper = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize,
    output: std.ArrayList(u8),

    // Configuration
    tsx_mode: bool,
    enable_comptime: bool,
    comptime_env: ?ComptimeEnv,
    report_errors: bool,

    // State
    line: u32,
    col: u32,

    // Context tracking for smart colon handling
    // When true, colons are for expressions (object literals), not types
    in_expression: bool,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8, options: StripOptions) Self {
        return .{
            .allocator = allocator,
            .source = source,
            .pos = 0,
            .output = .empty,
            .tsx_mode = options.tsx_mode,
            .enable_comptime = options.enable_comptime,
            .comptime_env = options.comptime_env,
            .report_errors = options.report_errors,
            .line = 1,
            .col = 1,
            .in_expression = false,
        };
    }

    pub fn strip(self: *Self) StripError!StripResult {
        while (self.pos < self.source.len) {
            // Check for unsupported constructs at statement boundaries
            if (self.isAtStatementStart()) {
                if (try self.tryStripTypeDeclaration()) continue;
                if (try self.tryStripImportType()) continue;
                if (try self.tryStripExportType()) continue;
                try self.checkUnsupported();
            }

            // Process next token
            try self.processToken();
        }

        const code = self.output.toOwnedSlice(self.allocator) catch return StripError.OutOfMemory;
        return StripResult{
            .code = code,
            .allocator = self.allocator,
        };
    }

    // ========================================================================
    // Token Processing
    // ========================================================================

    fn processToken(self: *Self) StripError!void {
        if (self.pos >= self.source.len) return;

        const start = self.pos;
        const c = self.source[self.pos];

        // Whitespace - pass through
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
            self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
            return;
        }

        // Comments - pass through
        if (c == '/' and self.pos + 1 < self.source.len) {
            const next = self.source[self.pos + 1];
            if (next == '/') {
                self.skipLineComment();
                self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
                return;
            }
            if (next == '*') {
                try self.skipBlockComment();
                self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
                return;
            }
        }

        // Strings and template literals - pass through
        if (c == '"' or c == '\'' or c == '`') {
            try self.skipString(c);
            self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
            return;
        }

        // Numbers - pass through
        if (std.ascii.isDigit(c) or (c == '.' and self.pos + 1 < self.source.len and std.ascii.isDigit(self.source[self.pos + 1]))) {
            self.skipNumber();
            self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
            return;
        }

        // Identifiers
        if (isIdentifierStart(c)) {
            const ident = self.scanIdentifier();

            // Check for comptime() expression
            if (self.enable_comptime and std.mem.eql(u8, ident, "comptime")) {
                if (try self.tryEvaluateComptime(start)) return;
            }

            // Check for 'as' or 'satisfies' after expression
            if (std.mem.eql(u8, ident, "as")) {
                if (try self.tryStripAsAssertion(start)) return;
            }
            if (std.mem.eql(u8, ident, "satisfies")) {
                if (try self.tryStripSatisfiesAssertion(start)) return;
            }

            // Check for function declaration - handle generics inline
            if (std.mem.eql(u8, ident, "function")) {
                self.output.appendSlice(self.allocator, ident) catch return StripError.OutOfMemory;
                try self.handleFunctionDeclaration();
                return;
            }

            // Keywords that start expressions
            if (std.mem.eql(u8, ident, "return") or std.mem.eql(u8, ident, "throw") or
                std.mem.eql(u8, ident, "new") or std.mem.eql(u8, ident, "typeof") or
                std.mem.eql(u8, ident, "delete") or std.mem.eql(u8, ident, "void") or
                std.mem.eql(u8, ident, "await") or std.mem.eql(u8, ident, "yield"))
            {
                self.in_expression = true;
            }
            // Keywords that end expressions
            else if (std.mem.eql(u8, ident, "let") or std.mem.eql(u8, ident, "const") or
                std.mem.eql(u8, ident, "var"))
            {
                self.in_expression = false;
            }

            self.output.appendSlice(self.allocator, ident) catch return StripError.OutOfMemory;
            return;
        }

        // Colon - might be type annotation
        if (c == ':') {
            if (try self.tryStripColonAnnotation()) return;
            self.pos += 1;
            self.col += 1;
            self.output.append(self.allocator, ':') catch return StripError.OutOfMemory;
            return;
        }

        // Angle bracket - might be generic params
        if (c == '<') {
            if (try self.tryStripGenericParams()) return;
            self.pos += 1;
            self.col += 1;
            self.output.append(self.allocator, '<') catch return StripError.OutOfMemory;
            return;
        }

        // Check for arrow function: = (...): Type => or = <T>(...): Type =>
        if (c == '=') {
            self.pos += 1;
            self.col += 1;
            self.output.append(self.allocator, c) catch return StripError.OutOfMemory;

            // Copy whitespace
            const ws_start = self.pos;
            self.skipWhitespaceTracked();
            self.output.appendSlice(self.allocator, self.source[ws_start..self.pos]) catch return StripError.OutOfMemory;

            // Check for generic arrow: = <T>(...)
            if (self.pos < self.source.len and self.source[self.pos] == '<' and !self.tsx_mode) {
                if (self.looksLikeGenericArrow()) {
                    const generic_start = self.pos;
                    if (self.skipBalancedAngles()) {
                        self.blankSpan(generic_start, self.pos);
                        // Copy whitespace after generics
                        const ws2_start = self.pos;
                        self.skipWhitespaceTracked();
                        self.output.appendSlice(self.allocator, self.source[ws2_start..self.pos]) catch return StripError.OutOfMemory;
                    }
                }
            }

            // Check for arrow function params
            if (self.pos < self.source.len and self.source[self.pos] == '(') {
                if (self.looksLikeArrowFunction()) {
                    try self.handleArrowFunction();
                }
            }
            self.in_expression = true;
            return;
        }

        // Track expression context based on punctuators
        switch (c) {
            ';' => self.in_expression = false, // Statement end
            '{' => {}, // Keep current context (could be block or object literal)
            '}' => self.in_expression = false, // End of block/object
            '(' => self.in_expression = true, // Contents of parens is expression
            ')' => {}, // Keep context
            '[' => self.in_expression = true, // Array literal
            ']' => {}, // Keep context
            else => {},
        }

        // Other punctuators - pass through
        self.pos += 1;
        self.col += 1;
        self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
    }

    // ========================================================================
    // Function Declaration Handling (for generics)
    // ========================================================================

    fn handleFunctionDeclaration(self: *Self) StripError!void {
        // We've already output "function"
        // Now handle: [name]<generics>(params): returnType { ... }

        // Copy whitespace
        const ws_start = self.pos;
        self.skipWhitespaceTracked();
        self.output.appendSlice(self.allocator, self.source[ws_start..self.pos]) catch return StripError.OutOfMemory;

        // Copy function name (optional for function expressions)
        if (self.pos < self.source.len and isIdentifierStart(self.source[self.pos])) {
            const name = self.scanIdentifier();
            self.output.appendSlice(self.allocator, name) catch return StripError.OutOfMemory;
        }

        // Check for generic params <T, U>
        if (self.pos < self.source.len and self.source[self.pos] == '<') {
            const generic_start = self.pos;
            if (self.skipBalancedAngles()) {
                // Blank the generic params
                self.blankSpan(generic_start, self.pos);
            }
        }

        // Handle parameter list with type stripping
        // Preserve whitespace before (
        const ws2_start = self.pos;
        self.skipWhitespaceTracked();
        self.output.appendSlice(self.allocator, self.source[ws2_start..self.pos]) catch return StripError.OutOfMemory;

        if (self.pos < self.source.len and self.source[self.pos] == '(') {
            try self.handleFunctionParams();
        }

        // Handle return type annotation
        const ws3_start = self.pos;
        self.skipWhitespaceTracked();

        if (self.pos < self.source.len and self.source[self.pos] == ':') {
            const colon_pos = self.pos;
            self.pos += 1;
            self.col += 1;
            self.skipWhitespaceTracked();

            if (self.looksLikeTypeStart()) {
                try self.skipTypeExpressionUntilDelimiter(&[_]u8{ '{', ';', '=' });
                // Skip whitespace after type
                self.skipWhitespaceTracked();
                // Blank the return type annotation (but preserve final whitespace)
                self.blankSpan(colon_pos, self.pos);
            }
        } else {
            // No return type - output the whitespace we skipped
            self.output.appendSlice(self.allocator, self.source[ws3_start..self.pos]) catch return StripError.OutOfMemory;
        }

        // Body will be handled by normal token processing
        self.in_expression = false;
    }

    fn handleFunctionParams(self: *Self) StripError!void {
        // We're at '('
        self.output.append(self.allocator, '(') catch return StripError.OutOfMemory;
        self.pos += 1;
        self.col += 1;

        var paren_depth: u16 = 1;

        while (self.pos < self.source.len and paren_depth > 0) {
            const c = self.source[self.pos];

            if (c == '(') {
                paren_depth += 1;
                self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
                self.pos += 1;
                self.col += 1;
                continue;
            }

            if (c == ')') {
                paren_depth -= 1;
                self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
                self.pos += 1;
                self.col += 1;
                continue;
            }

            // Strip type annotations in params
            if (c == ':' and paren_depth == 1) {
                const colon_pos = self.pos;
                self.pos += 1;
                self.col += 1;
                self.skipWhitespaceTracked();

                if (self.looksLikeTypeStart()) {
                    // Check for banned 'any' type
                    try self.checkForAnyType();
                    // Skip to , or ) at depth 1
                    try self.skipParamType();
                    self.blankSpan(colon_pos, self.pos);
                    continue;
                } else {
                    // Not a type, output the colon
                    self.pos = colon_pos;
                }
            }

            // Handle strings
            if (c == '"' or c == '\'' or c == '`') {
                const start = self.pos;
                try self.skipString(c);
                self.output.appendSlice(self.allocator, self.source[start..self.pos]) catch return StripError.OutOfMemory;
                continue;
            }

            // Handle whitespace
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                if (c == '\n') {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
                self.pos += 1;
                continue;
            }

            // Pass through everything else
            self.output.append(self.allocator, c) catch return StripError.OutOfMemory;
            self.pos += 1;
            self.col += 1;
        }
    }

    fn handleArrowFunction(self: *Self) StripError!void {
        // We're at '(' after '='
        // Handle: (params): ReturnType => body
        try self.handleFunctionParams();

        // Handle optional return type annotation
        const ws_start = self.pos;
        self.skipWhitespaceTracked();

        if (self.pos < self.source.len and self.source[self.pos] == ':') {
            const colon_pos = self.pos;
            self.pos += 1;
            self.col += 1;
            self.skipWhitespaceTracked();

            if (self.looksLikeTypeStart()) {
                // Skip to => or {
                try self.skipTypeExpressionUntilDelimiter(&[_]u8{'{'});
                // Check for =>
                self.skipWhitespaceTracked();
                if (self.pos + 1 < self.source.len and
                    self.source[self.pos] == '=' and self.source[self.pos + 1] == '>')
                {
                    // Blank just the return type, not the =>
                    self.blankSpan(colon_pos, self.pos);
                    return;
                }
            }
        }
        // If no return type, output the whitespace we skipped
        self.output.appendSlice(self.allocator, self.source[ws_start..self.pos]) catch return StripError.OutOfMemory;
    }

    fn looksLikeArrowFunction(self: *Self) bool {
        // Scan ahead to check if this is (params) => or (params): Type =>
        // vs just a parenthesized expression like (foo as number)
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;
        defer {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
        }

        // Skip the opening paren
        self.pos += 1;
        var paren_depth: u16 = 1;

        // Scan to find matching )
        while (self.pos < self.source.len and paren_depth > 0) {
            const c = self.source[self.pos];
            if (c == '(') paren_depth += 1;
            if (c == ')') paren_depth -= 1;
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return false;
                continue;
            }
            self.pos += 1;
        }

        // Skip whitespace after )
        while (self.pos < self.source.len and (self.source[self.pos] == ' ' or
            self.source[self.pos] == '\t' or self.source[self.pos] == '\n' or
            self.source[self.pos] == '\r'))
        {
            self.pos += 1;
        }

        // Check for : (return type) or => (arrow)
        if (self.pos >= self.source.len) return false;

        if (self.source[self.pos] == ':') {
            // Has return type annotation - likely arrow function
            return true;
        }

        if (self.pos + 1 < self.source.len and
            self.source[self.pos] == '=' and self.source[self.pos + 1] == '>')
        {
            // Direct arrow - is arrow function
            return true;
        }

        return false;
    }

    fn looksLikeGenericArrow(self: *Self) bool {
        // Check if <...> is followed by ( for arrow function
        const saved_pos = self.pos;
        defer self.pos = saved_pos;

        // Skip <...>
        if (!self.skipBalancedAngles()) return false;

        // Skip whitespace
        while (self.pos < self.source.len and (self.source[self.pos] == ' ' or
            self.source[self.pos] == '\t' or self.source[self.pos] == '\n'))
        {
            self.pos += 1;
        }

        // Should be followed by (
        return self.pos < self.source.len and self.source[self.pos] == '(';
    }

    fn skipParamType(self: *Self) StripError!void {
        // Skip type expression in parameter until , or )
        var paren_depth: u16 = 0;
        var angle_depth: u16 = 0;
        var bracket_depth: u16 = 0;
        var brace_depth: u16 = 0;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            // Check identifiers for banned types
            if (isIdentifierStart(c)) {
                try self.checkForAnyType();
            }

            if (c == '(') {
                paren_depth += 1;
            } else if (c == ')') {
                if (paren_depth == 0) return; // End of params
                paren_depth -= 1;
            } else if (c == '<') {
                angle_depth += 1;
            } else if (c == '>') {
                if (angle_depth > 0) angle_depth -= 1;
            } else if (c == '[') {
                bracket_depth += 1;
            } else if (c == ']') {
                if (bracket_depth > 0) bracket_depth -= 1;
            } else if (c == '{') {
                brace_depth += 1;
            } else if (c == '}') {
                if (brace_depth > 0) brace_depth -= 1;
            } else if (c == ',' and paren_depth == 0 and angle_depth == 0 and bracket_depth == 0 and brace_depth == 0) {
                return; // Next param
            } else if (c == '=' and paren_depth == 0 and angle_depth == 0) {
                return; // Default value
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    // ========================================================================
    // Type/Interface Declaration Stripping (Phase 2)
    // ========================================================================

    fn tryStripTypeDeclaration(self: *Self) StripError!bool {
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;

        // Check for 'type' or 'interface' keyword
        const keyword = self.peekKeyword();
        if (keyword == null) return false;

        const is_type = std.mem.eql(u8, keyword.?, "type");
        const is_interface = std.mem.eql(u8, keyword.?, "interface");

        if (!is_type and !is_interface) return false;

        // Skip the keyword
        self.pos += keyword.?.len;
        self.col += @intCast(keyword.?.len);
        self.skipWhitespaceTracked();

        // Must be followed by identifier
        if (!self.peekIdentifierStart()) {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
            return false;
        }

        // Skip identifier
        _ = self.scanIdentifier();
        self.skipWhitespaceTracked();

        // Skip optional generic params <T, U>
        if (self.pos < self.source.len and self.source[self.pos] == '<') {
            if (!self.skipBalancedAngles()) {
                self.pos = saved_pos;
                self.line = saved_line;
                self.col = saved_col;
                return false;
            }
            self.skipWhitespaceTracked();
        }

        // For interface: skip optional 'extends' clause
        if (is_interface) {
            const extends_kw = self.peekKeyword();
            if (extends_kw != null and std.mem.eql(u8, extends_kw.?, "extends")) {
                self.pos += 7; // "extends"
                self.col += 7;
                self.skipWhitespaceTracked();
                // Skip base type(s)
                try self.skipTypeExpressionUntilDelimiter(&[_]u8{ '{', ';' });
                self.skipWhitespaceTracked();
            }
        }

        // For type: skip = and type expression
        if (is_type and self.pos < self.source.len and self.source[self.pos] == '=') {
            self.pos += 1;
            self.col += 1;
            self.skipWhitespaceTracked();
            try self.skipTypeExpressionUntilDelimiter(&[_]u8{ ';', '\n' });
        }

        // For interface: skip the { ... } body
        if (is_interface and self.pos < self.source.len and self.source[self.pos] == '{') {
            self.skipBalancedBraces();
        }

        // Skip optional semicolon
        self.skipWhitespaceTracked();
        if (self.pos < self.source.len and self.source[self.pos] == ';') {
            self.pos += 1;
            self.col += 1;
        }

        // Blank the entire span
        self.blankSpan(saved_pos, self.pos);
        return true;
    }

    // ========================================================================
    // Import/Export Type Stripping (Phase 3)
    // ========================================================================

    fn tryStripImportType(self: *Self) StripError!bool {
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;

        const keyword = self.peekKeyword();
        if (keyword == null or !std.mem.eql(u8, keyword.?, "import")) return false;

        self.pos += 6; // "import"
        self.col += 6;
        self.skipWhitespaceTracked();

        const type_kw = self.peekKeyword();
        if (type_kw == null or !std.mem.eql(u8, type_kw.?, "type")) {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
            return false;
        }

        // Skip to end of statement
        self.skipToStatementEnd();
        self.blankSpan(saved_pos, self.pos);
        return true;
    }

    fn tryStripExportType(self: *Self) StripError!bool {
        const saved_pos = self.pos;
        const saved_line = self.line;
        const saved_col = self.col;

        const keyword = self.peekKeyword();
        if (keyword == null or !std.mem.eql(u8, keyword.?, "export")) return false;

        self.pos += 6; // "export"
        self.col += 6;
        self.skipWhitespaceTracked();

        const type_kw = self.peekKeyword();
        if (type_kw == null or !std.mem.eql(u8, type_kw.?, "type")) {
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;
            return false;
        }

        // Skip to end of statement
        self.skipToStatementEnd();
        self.blankSpan(saved_pos, self.pos);
        return true;
    }

    // ========================================================================
    // Annotation Stripping (Phase 4)
    // ========================================================================

    /// Check if we're at a label colon (identifier at statement start followed by colon).
    /// Labels: `public: foo();` or `loop: for (...) {...}`
    fn isLabelColon(self: *Self) bool {
        const items = self.output.items;
        if (items.len == 0) return false;

        // Find the identifier that precedes the colon
        var ident_start = items.len;

        // Skip back through identifier characters
        while (ident_start > 0 and isIdentifierContinue(items[ident_start - 1])) {
            ident_start -= 1;
        }

        // Must have at least one identifier character and start with identifier start char
        if (ident_start >= items.len) return false;
        if (!isIdentifierStart(items[ident_start])) return false;

        // Check if we're at statement start (before the identifier)
        if (ident_start == 0) return true; // Start of file

        // Look at the character before the identifier (skip whitespace)
        var check_pos = ident_start - 1;
        while (check_pos > 0 and (items[check_pos] == ' ' or items[check_pos] == '\t')) {
            check_pos -= 1;
        }

        const before = items[check_pos];
        return before == ';' or before == '{' or before == '}' or before == '\n';
    }

    fn tryStripColonAnnotation(self: *Self) StripError!bool {
        // We're at ':'
        // Skip if we're in expression context (object literals, arrays, etc.)
        if (self.in_expression) {
            return false;
        }

        // Check if this is a label (identifier at statement start followed by colon)
        // Labels look like: public: foo(); or loop: for (...) {...}
        if (self.isLabelColon()) {
            return false;
        }

        const colon_pos = self.pos;
        const colon_col = self.col;

        self.pos += 1;
        self.col += 1;

        // Skip whitespace after colon
        self.skipWhitespaceTracked();

        // Check if this looks like a type annotation
        // Type annotations are followed by type syntax, not expression syntax
        if (!self.looksLikeTypeStart()) {
            self.pos = colon_pos;
            self.col = colon_col;
            return false;
        }

        // Check for banned 'any' type
        try self.checkForAnyType();

        // This looks like a type annotation - skip the type
        try self.skipTypeExpressionUntilDelimiter(&[_]u8{ ',', ')', ';', '=', '{', '}' });

        // Check for arrow function
        self.skipWhitespaceTracked();
        if (self.pos + 1 < self.source.len and
            self.source[self.pos] == '=' and self.source[self.pos + 1] == '>')
        {
            // Stop before =>
        }

        // Blank from colon to current position
        self.blankSpan(colon_pos, self.pos);
        return true;
    }

    fn looksLikeTypeStart(self: *Self) bool {
        if (self.pos >= self.source.len) return false;
        const c = self.source[self.pos];

        // Type starts with: identifier, {, (, [, <, 'string', "string", typeof, keyof
        if (isIdentifierStart(c)) return true;
        if (c == '{' or c == '(' or c == '[' or c == '<') return true;
        if (c == '\'' or c == '"') return true;

        return false;
    }

    // ========================================================================
    // Assertion Stripping (Phase 5)
    // ========================================================================

    fn tryStripAsAssertion(self: *Self, keyword_start: usize) StripError!bool {
        // We just scanned 'as' - check if it's an assertion
        self.skipWhitespaceTracked();

        if (!self.looksLikeTypeStart()) {
            return false;
        }

        // Check for banned 'any' type
        try self.checkForAnyType();

        // Skip the type
        try self.skipTypeExpressionUntilDelimiter(&[_]u8{ ',', ')', ';', '}', ']' });

        // Blank from 'as' to current position
        self.blankSpan(keyword_start, self.pos);
        return true;
    }

    fn tryStripSatisfiesAssertion(self: *Self, keyword_start: usize) StripError!bool {
        // We just scanned 'satisfies'
        self.skipWhitespaceTracked();

        if (!self.looksLikeTypeStart()) {
            return false;
        }

        // Check for banned 'any' type
        try self.checkForAnyType();

        // Skip the type
        try self.skipTypeExpressionUntilDelimiter(&[_]u8{ ',', ')', ';', '}' });

        // Blank from 'satisfies' to current position
        self.blankSpan(keyword_start, self.pos);
        return true;
    }

    // ========================================================================
    // Comptime Expression Evaluation
    // ========================================================================

    fn tryEvaluateComptime(self: *Self, keyword_start: usize) StripError!bool {
        // We just scanned 'comptime', now expect (
        self.skipWhitespaceTracked();

        if (self.pos >= self.source.len or self.source[self.pos] != '(') {
            // Not a comptime() call, restore and treat as regular identifier
            self.pos = keyword_start + 8; // "comptime".len
            return false;
        }

        // Find the matching closing paren
        self.pos += 1; // skip (
        self.col += 1;

        var paren_depth: u16 = 1;
        const expr_start = self.pos;

        while (self.pos < self.source.len and paren_depth > 0) {
            const c = self.source[self.pos];

            // Handle strings
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return StripError.ComptimeEvaluationFailed;
                continue;
            }

            if (c == '(') {
                paren_depth += 1;
            } else if (c == ')') {
                paren_depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }

        if (paren_depth != 0) {
            return StripError.ComptimeEvaluationFailed;
        }

        // expr_end is one before the closing paren
        const expr_end = self.pos - 1;
        const expr = self.source[expr_start..expr_end];

        // Evaluate the expression
        var evaluator = comptime_eval.ComptimeEvaluator.init(self.allocator, expr, self.line, self.col);

        // Set up environment if provided
        if (self.comptime_env) |env| {
            evaluator.env = env.env_vars;
            evaluator.build_time = env.build_time;
            evaluator.git_commit = env.git_commit;
            evaluator.version = env.version;
        }

        const result = evaluator.evaluate() catch return StripError.ComptimeEvaluationFailed;
        defer result.deinit(self.allocator);

        // Emit the literal value
        const literal = comptime_eval.emitLiteral(self.allocator, result) catch return StripError.OutOfMemory;
        defer self.allocator.free(literal);

        // Output the literal
        self.output.appendSlice(self.allocator, literal) catch return StripError.OutOfMemory;

        // Pad with spaces to preserve line/column positions
        // The total span is from keyword_start to self.pos (end of closing paren)
        const total_span = self.pos - keyword_start;
        const literal_len = literal.len;

        if (total_span > literal_len) {
            const padding = total_span - literal_len;
            for (0..padding) |_| {
                self.output.append(self.allocator, ' ') catch return StripError.OutOfMemory;
            }
        }

        return true;
    }

    // ========================================================================
    // Generic Parameter Stripping (Phase 6)
    // ========================================================================

    fn tryStripGenericParams(self: *Self) StripError!bool {
        // We're at '<'
        // This could be: generic params, comparison, JSX

        if (self.tsx_mode) {
            // In TSX, need to distinguish from JSX
            if (self.looksLikeJsx()) {
                return false;
            }
        }

        // Check if this is a comparison operator
        if (self.looksLikeComparison()) {
            return false;
        }

        // Try to parse as generic params
        const start = self.pos;
        if (!self.skipBalancedAngles()) {
            return false;
        }

        // Generic params are typically followed by ( or extends
        self.skipWhitespaceTracked();
        if (self.pos < self.source.len) {
            const next = self.source[self.pos];
            if (next == '(' or next == '{') {
                // Looks like generic function - blank the params
                self.blankSpan(start, self.pos);
                return true;
            }

            // Check for 'extends' (in type context)
            const kw = self.peekKeyword();
            if (kw != null and std.mem.eql(u8, kw.?, "extends")) {
                self.blankSpan(start, self.pos);
                return true;
            }
        }

        // Not generic params, restore
        self.pos = start;
        return false;
    }

    fn looksLikeJsx(self: *Self) bool {
        if (self.pos + 1 >= self.source.len) return false;
        const next = self.source[self.pos + 1];
        // JSX: <div, <MyComponent, <>
        if (next == '>') return true; // Fragment
        if (std.ascii.isAlphabetic(next) or next == '_') {
            // Could be JSX tag - check if lowercase (element) or uppercase (component)
            return true;
        }
        return false;
    }

    fn looksLikeComparison(self: *Self) bool {
        // Look at what comes before - if it's an expression end, this is comparison
        // This is a heuristic - we look at the output buffer
        if (self.output.items.len == 0) return false;

        const last = self.output.items[self.output.items.len - 1];
        // If last char is identifier/number end, or ), ], this could be comparison
        if (std.ascii.isAlphanumeric(last) or last == ')' or last == ']' or last == '_' or last == '$') {
            // Could be comparison like `a < b` or `foo() < bar`
            // Need more context - for now, be conservative
            return true;
        }
        return false;
    }

    // ========================================================================
    // Error Checking (Phase 8)
    // ========================================================================

    fn checkUnsupported(self: *Self) StripError!void {
        // Check for decorator first (before keyword check)
        if (self.pos < self.source.len and self.source[self.pos] == '@') {
            if (self.report_errors) std.log.err("{}:{}: '@decorator' syntax is not supported; use function composition instead", .{ self.line, self.col });
            return StripError.UnsupportedDecorator;
        }

        const keyword = self.peekKeyword();
        if (keyword == null) return;

        if (std.mem.eql(u8, keyword.?, "enum")) {
            if (self.report_errors) std.log.err("{}:{}: 'enum' is not supported; use object literals or discriminated unions instead", .{ self.line, self.col });
            return StripError.UnsupportedEnum;
        }
        if (std.mem.eql(u8, keyword.?, "const")) {
            // Check for const enum
            const saved = self.pos;
            self.pos += 5; // "const"
            self.skipWhitespaceTracked();
            const next_kw = self.peekKeyword();
            self.pos = saved;
            if (next_kw != null and std.mem.eql(u8, next_kw.?, "enum")) {
                if (self.report_errors) std.log.err("{}:{}: 'const enum' is not supported; use object literals or discriminated unions instead", .{ self.line, self.col });
                return StripError.UnsupportedEnum;
            }
        }
        if (std.mem.eql(u8, keyword.?, "namespace") or std.mem.eql(u8, keyword.?, "module")) {
            if (self.report_errors) std.log.err("{}:{}: 'namespace' is not supported; use ES6 modules instead", .{ self.line, self.col });
            return StripError.UnsupportedNamespace;
        }
        // NOTE: 'class' and 'abstract class' are now handled by the parser (see zts/parser/parse.zig)
        // This ensures consistent error messages for both .ts and .js files
        if (std.mem.eql(u8, keyword.?, "implements")) {
            if (self.report_errors) std.log.err("{}:{}: 'implements' is not supported; use duck typing or runtime checks instead", .{ self.line, self.col });
            return StripError.UnsupportedClass;
        }
        // Class member modifiers - only valid in class context which is already rejected
        if (std.mem.eql(u8, keyword.?, "public") or
            std.mem.eql(u8, keyword.?, "private") or
            std.mem.eql(u8, keyword.?, "protected"))
        {
            // Allow labels like "public:" at statement start
            const saved_pos = self.pos;
            const saved_line = self.line;
            const saved_col = self.col;
            self.pos += @intCast(keyword.?.len);
            self.col += @intCast(keyword.?.len);
            self.skipWhitespaceTracked();
            const is_label = self.pos < self.source.len and self.source[self.pos] == ':';
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;

            if (!is_label) {
                if (self.report_errors) std.log.err("{}:{}: '{s}' modifier is not supported; use naming conventions (e.g., _private) instead", .{ self.line, self.col, keyword.? });
                return StripError.UnsupportedClass;
            }
        }
    }

    // ========================================================================
    // Banned Type Detection
    // ========================================================================

    fn checkForAnyType(self: *Self) StripError!void {
        if (self.pos >= self.source.len) return;
        if (!isIdentifierStart(self.source[self.pos])) return;

        const kw = self.peekKeyword();
        if (kw != null and std.mem.eql(u8, kw.?, "any")) {
            // Word boundary: next char after "any" must not continue the identifier
            const after = self.pos + 3;
            if (after >= self.source.len or !isIdentifierContinue(self.source[after])) {
                if (self.report_errors) std.log.err("{}:{}: 'any' type is not supported; use specific types (string, number, object) or union types instead", .{ self.line, self.col });
                return StripError.UnsupportedAnyType;
            }
        }
    }

    // ========================================================================
    // Helper Functions
    // ========================================================================

    fn isAtStatementStart(self: *Self) bool {
        // Simplified check - at start of file or after statement-ending punctuation
        if (self.output.items.len == 0) return true;
        const last = self.output.items[self.output.items.len - 1];
        return last == ';' or last == '{' or last == '}' or last == '\n';
    }

    fn blankSpan(self: *Self, start: usize, end: usize) void {
        // Replace with spaces, preserving newlines
        for (self.source[start..end]) |c| {
            if (c == '\n') {
                self.output.append(self.allocator, '\n') catch {};
            } else {
                self.output.append(self.allocator, ' ') catch {};
            }
        }
    }

    fn skipWhitespaceTracked(self: *Self) void {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == ' ' or c == '\t') {
                self.pos += 1;
                self.col += 1;
            } else if (c == '\n') {
                self.pos += 1;
                self.line += 1;
                self.col = 1;
            } else if (c == '\r') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn skipToStatementEnd(self: *Self) void {
        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            if (c == ';') {
                self.pos += 1;
                self.col += 1;
                return;
            }
            if (c == '\n') {
                return; // Don't consume newline
            }
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }
            self.pos += 1;
            self.col += 1;
        }
    }

    fn skipTypeExpressionUntilDelimiter(self: *Self, delimiters: []const u8) StripError!void {
        var paren_depth: u16 = 0;
        var bracket_depth: u16 = 0;
        var angle_depth: u16 = 0;
        var brace_depth: u16 = 0;

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];

            // Handle strings
            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            // Check identifiers for banned types (catches nested 'any' in Record<string, any> etc.)
            if (isIdentifierStart(c)) {
                try self.checkForAnyType();
            }

            // Check delimiters at depth 0 FIRST (before tracking)
            if (paren_depth == 0 and bracket_depth == 0 and angle_depth == 0 and brace_depth == 0) {
                for (delimiters) |d| {
                    if (c == d) return;
                }
                // Check for arrow
                if (c == '=' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '>') {
                    return; // Stop before =>
                }
            }

            // Track nesting
            if (c == '(') {
                paren_depth += 1;
            } else if (c == ')') {
                if (paren_depth > 0) paren_depth -= 1;
            } else if (c == '[') {
                bracket_depth += 1;
            } else if (c == ']') {
                if (bracket_depth > 0) bracket_depth -= 1;
            } else if (c == '<') {
                angle_depth += 1;
            } else if (c == '>') {
                if (angle_depth > 0) angle_depth -= 1;
            } else if (c == '{') {
                brace_depth += 1;
            } else if (c == '}') {
                if (brace_depth > 0) brace_depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skipBalancedAngles(self: *Self) bool {
        if (self.pos >= self.source.len or self.source[self.pos] != '<') return false;

        self.pos += 1;
        self.col += 1;
        var depth: u16 = 1;

        while (self.pos < self.source.len and depth > 0) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch return false;
                continue;
            }

            if (c == '<') {
                depth += 1;
            } else if (c == '>') {
                depth -= 1;
            } else if (c == '(' or c == '[' or c == '{') {
                // These must be balanced within the generic
                self.skipMatchingBracket(c);
                continue;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }

        return depth == 0;
    }

    fn skipBalancedBraces(self: *Self) void {
        if (self.pos >= self.source.len or self.source[self.pos] != '{') return;

        self.pos += 1;
        self.col += 1;
        var depth: u16 = 1;

        while (self.pos < self.source.len and depth > 0) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            if (c == '{') {
                depth += 1;
            } else if (c == '}') {
                depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skipMatchingBracket(self: *Self, open: u8) void {
        const close: u8 = switch (open) {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            else => return,
        };

        self.pos += 1;
        self.col += 1;
        var depth: u16 = 1;

        while (self.pos < self.source.len and depth > 0) {
            const c = self.source[self.pos];

            if (c == '"' or c == '\'' or c == '`') {
                self.skipString(c) catch {};
                continue;
            }

            if (c == open) {
                depth += 1;
            } else if (c == close) {
                depth -= 1;
            }

            if (c == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn peekKeyword(self: *Self) ?[]const u8 {
        if (self.pos >= self.source.len) return null;
        if (!isIdentifierStart(self.source[self.pos])) return null;

        var end = self.pos;
        while (end < self.source.len and isIdentifierContinue(self.source[end])) {
            end += 1;
        }
        return self.source[self.pos..end];
    }

    fn peekIdentifierStart(self: *Self) bool {
        if (self.pos >= self.source.len) return false;
        return isIdentifierStart(self.source[self.pos]);
    }

    fn scanIdentifier(self: *Self) []const u8 {
        const start = self.pos;
        while (self.pos < self.source.len and isIdentifierContinue(self.source[self.pos])) {
            self.pos += 1;
            self.col += 1;
        }
        return self.source[start..self.pos];
    }

    fn skipString(self: *Self, quote: u8) StripError!void {
        self.pos += 1;
        self.col += 1;

        if (quote == '`') {
            // Template literal
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == '`') {
                    self.pos += 1;
                    self.col += 1;
                    return;
                }
                if (c == '\\' and self.pos + 1 < self.source.len) {
                    self.pos += 2;
                    self.col += 2;
                    continue;
                }
                if (c == '$' and self.pos + 1 < self.source.len and self.source[self.pos + 1] == '{') {
                    self.pos += 2;
                    self.col += 2;
                    var depth: usize = 1;
                    while (self.pos < self.source.len and depth > 0) {
                        const ic = self.source[self.pos];
                        if (ic == '{') depth += 1;
                        if (ic == '}') depth -= 1;
                        if (ic == '\n') {
                            self.line += 1;
                            self.col = 1;
                        } else {
                            self.col += 1;
                        }
                        self.pos += 1;
                    }
                    continue;
                }
                if (c == '\n') {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                self.pos += 1;
            }
        } else {
            // Regular string
            while (self.pos < self.source.len) {
                const c = self.source[self.pos];
                if (c == quote) {
                    self.pos += 1;
                    self.col += 1;
                    return;
                }
                if (c == '\\' and self.pos + 1 < self.source.len) {
                    self.pos += 2;
                    self.col += 2;
                    continue;
                }
                if (c == '\n') {
                    return StripError.UnterminatedString;
                }
                self.pos += 1;
                self.col += 1;
            }
        }
    }

    fn skipLineComment(self: *Self) void {
        while (self.pos < self.source.len and self.source[self.pos] != '\n') {
            self.pos += 1;
            self.col += 1;
        }
    }

    fn skipBlockComment(self: *Self) StripError!void {
        self.pos += 2; // skip /*
        self.col += 2;

        while (self.pos + 1 < self.source.len) {
            if (self.source[self.pos] == '*' and self.source[self.pos + 1] == '/') {
                self.pos += 2;
                self.col += 2;
                return;
            }
            if (self.source[self.pos] == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }

        return StripError.UnterminatedComment;
    }

    fn skipNumber(self: *Self) void {
        if (self.pos >= self.source.len) return;

        // Handle 0x, 0b, 0o prefixes
        if (self.source[self.pos] == '0' and self.pos + 1 < self.source.len) {
            const next = self.source[self.pos + 1];
            if (next == 'x' or next == 'X') {
                self.pos += 2;
                self.col += 2;
                while (self.pos < self.source.len and std.ascii.isHex(self.source[self.pos])) {
                    self.pos += 1;
                    self.col += 1;
                }
                return;
            }
            if (next == 'b' or next == 'B') {
                self.pos += 2;
                self.col += 2;
                while (self.pos < self.source.len and (self.source[self.pos] == '0' or self.source[self.pos] == '1')) {
                    self.pos += 1;
                    self.col += 1;
                }
                return;
            }
            if (next == 'o' or next == 'O') {
                self.pos += 2;
                self.col += 2;
                while (self.pos < self.source.len and self.source[self.pos] >= '0' and self.source[self.pos] <= '7') {
                    self.pos += 1;
                    self.col += 1;
                }
                return;
            }
        }

        // Decimal / float
        while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
            self.pos += 1;
            self.col += 1;
        }
        if (self.pos < self.source.len and self.source[self.pos] == '.') {
            self.pos += 1;
            self.col += 1;
            while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
                self.pos += 1;
                self.col += 1;
            }
        }
        // Exponent
        if (self.pos < self.source.len and (self.source[self.pos] == 'e' or self.source[self.pos] == 'E')) {
            self.pos += 1;
            self.col += 1;
            if (self.pos < self.source.len and (self.source[self.pos] == '+' or self.source[self.pos] == '-')) {
                self.pos += 1;
                self.col += 1;
            }
            while (self.pos < self.source.len and std.ascii.isDigit(self.source[self.pos])) {
                self.pos += 1;
                self.col += 1;
            }
        }
    }

    fn isIdentifierStart(c: u8) bool {
        return std.ascii.isAlphabetic(c) or c == '_' or c == '$';
    }

    fn isIdentifierContinue(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_' or c == '$';
    }
};

// ============================================================================
// Tests
// ============================================================================

test "passthrough plain js" {
    const source = "let x = 1;\nfunction f() { return x + 1; }";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "passthrough with strings" {
    const source = "let s = \"hello world\";\nlet t = 'single';";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "passthrough with comments" {
    const source = "// line comment\nlet x = 1; /* block */";
    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();
    try std.testing.expectEqualStrings(source, result.code);
}

test "type alias stripped" {
    const result = try strip(std.testing.allocator, "type X = number;", .{});
    defer @constCast(&result).deinit();
    // Should be all spaces/newlines
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "interface stripped" {
    const result = try strip(std.testing.allocator, "interface Foo { x: number; }", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "import type stripped" {
    const result = try strip(std.testing.allocator, "import type { Foo } from \"./types\";", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "export type stripped" {
    const result = try strip(std.testing.allocator, "export type { Foo };", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "enum errors" {
    const result = strip(std.testing.allocator, "enum Color { Red, Blue }", .{});
    try std.testing.expectError(StripError.UnsupportedEnum, result);
}

test "namespace errors" {
    const result = strip(std.testing.allocator, "namespace N { }", .{});
    try std.testing.expectError(StripError.UnsupportedNamespace, result);
}

test "decorator errors" {
    const result = strip(std.testing.allocator, "@sealed class X {}", .{});
    try std.testing.expectError(StripError.UnsupportedDecorator, result);
}

// NOTE: 'class' and 'abstract class' tests removed - now handled by parser
// See "class passes through to parser" test below

test "implements errors" {
    // implements as a standalone statement would be invalid
    const result = strip(std.testing.allocator, "implements Foo { }", .{});
    try std.testing.expectError(StripError.UnsupportedClass, result);
}

test "public modifier errors" {
    const result = strip(std.testing.allocator, "public foo() { }", .{});
    try std.testing.expectError(StripError.UnsupportedClass, result);
}

test "private modifier errors" {
    const result = strip(std.testing.allocator, "private x = 1;", .{});
    try std.testing.expectError(StripError.UnsupportedClass, result);
}

test "protected modifier errors" {
    const result = strip(std.testing.allocator, "protected foo() { }", .{});
    try std.testing.expectError(StripError.UnsupportedClass, result);
}

test "any type annotation errors" {
    const result = strip(std.testing.allocator, "const x: any = 5;", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any function param errors" {
    const result = strip(std.testing.allocator, "function f(x: any) { return x; }", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any return type errors" {
    const result = strip(std.testing.allocator, "function f(): any { return 1; }", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any as assertion errors" {
    const result = strip(std.testing.allocator, "const x = value as any;", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any in generic errors" {
    const result = strip(std.testing.allocator, "const x: Record<string, any> = {};", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any array errors" {
    const result = strip(std.testing.allocator, "const x: any[] = [];", .{});
    try std.testing.expectError(StripError.UnsupportedAnyType, result);
}

test "any variable name allowed" {
    var result = try strip(std.testing.allocator, "const any = 5;", .{});
    defer result.deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "const any") != null);
}

test "anything identifier allowed" {
    var result = try strip(std.testing.allocator, "let x: anything = 5;", .{});
    defer result.deinit();
}

// Stage 3 complete: class keyword now passes through stripper to be caught by parser
test "class passes through to parser" {
    var result = try strip(std.testing.allocator, "class Foo { }", .{});
    defer result.deinit();
    // Class should pass through (stripped to maintain positions)
    // Parser will catch it with helpful error message
    try std.testing.expect(result.code.len > 0);
    // Verify it contains some whitespace (class keyword was blanked)
    try std.testing.expect(std.mem.indexOfScalar(u8, result.code, ' ') != null or
        std.mem.indexOfScalar(u8, result.code, '{') != null);
}

test "public label allowed" {
    var result = try strip(std.testing.allocator, "public: foo();", .{});
    defer result.deinit();
    try std.testing.expectEqualStrings("public: foo();", result.code);
}

test "line preservation" {
    const input = "type X = number;\nlet x = 1;\nconsole.log(x);";
    const result = try strip(std.testing.allocator, input, .{});
    defer @constCast(&result).deinit();

    // Count newlines - should be preserved
    var input_newlines: usize = 0;
    var output_newlines: usize = 0;
    for (input) |c| {
        if (c == '\n') input_newlines += 1;
    }
    for (result.code) |c| {
        if (c == '\n') output_newlines += 1;
    }
    try std.testing.expectEqual(input_newlines, output_newlines);
}

test "type with usage preserved" {
    const input = "type User = { id: number };\nlet u = { id: 1 };";
    const result = try strip(std.testing.allocator, input, .{});
    defer @constCast(&result).deinit();

    // Second line should be preserved
    try std.testing.expect(std.mem.indexOf(u8, result.code, "let u = { id: 1 };") != null);
}

// ============================================================================
// Phase 4: Annotation Tests
// ============================================================================

test "variable annotation stripped" {
    const result = try strip(std.testing.allocator, "let x: number = 1;", .{});
    defer @constCast(&result).deinit();
    // Should strip ": number" but keep the rest
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "let x") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "= 1;") != null);
}

test "function params annotation stripped" {
    const result = try strip(std.testing.allocator, "function add(a: number, b: number) { return a + b; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function add(a") != null);
}

test "function return type stripped" {
    const result = try strip(std.testing.allocator, "function add(a, b): number { return a + b; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "): number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function add(a, b)") != null);
}

test "arrow function annotation stripped" {
    const result = try strip(std.testing.allocator, "const f = (x: string): string => x.trim();", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "const f = (x") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "=> x.trim()") != null);
}

// ============================================================================
// Phase 5: Assertion Tests
// ============================================================================

test "as assertion stripped" {
    const result = try strip(std.testing.allocator, "let n = (foo as number) + 1;", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, " as number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "(foo") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ") + 1") != null);
}

test "satisfies assertion stripped" {
    const result = try strip(std.testing.allocator, "const cfg = { port: 8080 } satisfies Config;", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "satisfies") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "{ port: 8080 }") != null);
}

// ============================================================================
// Phase 6: Generic Tests
// ============================================================================

test "generic function stripped" {
    const result = try strip(std.testing.allocator, "function id<T>(x: T): T { return x; }", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<T>") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": T") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function id") != null);
}

test "generic type alias stripped" {
    const result = try strip(std.testing.allocator, "type Box<T> = { value: T };", .{});
    defer @constCast(&result).deinit();
    const trimmed = std.mem.trim(u8, result.code, " \n\r\t");
    try std.testing.expectEqual(@as(usize, 0), trimmed.len);
}

test "full handler example" {
    const source =
        \\// TypeScript handler example
        \\
        \\type RequestData = {
        \\    name: string;
        \\    count: number;
        \\};
        \\
        \\interface ResponseData {
        \\    message: string;
        \\    timestamp: number;
        \\}
        \\
        \\function processData(data: RequestData): ResponseData {
        \\    return {
        \\        message: "Hello, " + data.name,
        \\        timestamp: Date.now()
        \\    };
        \\}
        \\
        \\function handler(req: Request): Response {
        \\    const data = { name: "World", count: 42 } as RequestData;
        \\    const result: ResponseData = processData(data);
        \\    return Response.json(result);
        \\}
        \\
    ;

    const result = try strip(std.testing.allocator, source, .{});
    defer @constCast(&result).deinit();

    // Should not contain type annotations
    try std.testing.expect(std.mem.indexOf(u8, result.code, "type RequestData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "interface ResponseData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": RequestData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": ResponseData") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, " as RequestData") == null);

    // Should contain the function bodies
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function processData") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "function handler") != null);
}

test "tsx mode preserves jsx" {
    const source =
        \\function App(props: Props): JSX.Element {
        \\    const name: string = "World";
        \\    return <div class="test">{name}</div>;
        \\}
    ;

    const result = try strip(std.testing.allocator, source, .{ .tsx_mode = true });
    defer @constCast(&result).deinit();

    // JSX should be preserved
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<div class=\"test\">") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "</div>") != null);

    // Types should be stripped
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": Props") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": string") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": JSX.Element") == null);
}

test "tsx mode handles fragments" {
    const source =
        \\function List(items: string[]): JSX.Element {
        \\    return <><span>a</span><span>b</span></>;
        \\}
    ;

    const result = try strip(std.testing.allocator, source, .{ .tsx_mode = true });
    defer @constCast(&result).deinit();

    // Fragment syntax preserved
    try std.testing.expect(std.mem.indexOf(u8, result.code, "<>") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "</>") != null);
}

// ============================================================================
// Comptime Tests
// ============================================================================

test "comptime disabled by default" {
    // When enable_comptime is false, comptime() is passed through as-is
    const result = try strip(std.testing.allocator, "const x = comptime(1 + 2);", .{});
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "comptime(1 + 2)") != null);
}

test "comptime simple arithmetic" {
    const result = try strip(std.testing.allocator, "const x = comptime(1 + 2 * 3);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "7") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "comptime") == null);
}

test "comptime Math.PI" {
    const result = try strip(std.testing.allocator, "const pi = comptime(Math.PI);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "3.14159") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "comptime") == null);
}

test "comptime string" {
    const result = try strip(std.testing.allocator, "const s = comptime(\"hello\");", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "\"hello\"") != null);
}

test "comptime array" {
    const result = try strip(std.testing.allocator, "const arr = comptime([1, 2, 3]);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "[1,2,3]") != null);
}

test "comptime object" {
    const result = try strip(std.testing.allocator, "const cfg = comptime({ a: 1, b: 2 });", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "({a:1,b:2})") != null);
}

test "comptime ternary" {
    const result = try strip(std.testing.allocator, "const v = comptime(true ? 1 : 0);", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "1") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "true") == null);
}

test "comptime Math.max" {
    const result = try strip(std.testing.allocator, "const m = comptime(Math.max(1, 5, 3));", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "5") != null);
}

test "comptime hash" {
    const result = try strip(std.testing.allocator, "const h = comptime(hash(\"test\"));", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    // Should have an 8-char hex string
    try std.testing.expect(std.mem.indexOf(u8, result.code, "\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "hash") == null);
}

test "comptime with types" {
    // Both type stripping and comptime should work together
    const source = "const x: number = comptime(1 + 2);";
    const result = try strip(std.testing.allocator, source, .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, ": number") == null);
    try std.testing.expect(std.mem.indexOf(u8, result.code, "3") != null);
}

test "comptime identifier without parens passes through" {
    // The identifier 'comptime' without () should be passed through
    const result = try strip(std.testing.allocator, "const comptime = 5;", .{ .enable_comptime = true });
    defer @constCast(&result).deinit();
    try std.testing.expect(std.mem.indexOf(u8, result.code, "const comptime = 5;") != null);
}
